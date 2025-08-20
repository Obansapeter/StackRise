
;; Contract: StackRise
;; Purpose: Milestone-based crowdfunding with weighted backer voting,
;;           arbiter fallback, refunds, fees, stretch goals and badges.
;; 
;; License : MIT
;; ====================================================================

;; Helper for handling campaign data
(define-private (get-campaign-data (campaign-id uint))
  (match (map-get? campaigns campaign-id)
    campaign (ok campaign)
    (err ERR-NOT-FOUND)))

;; Helper for handling milestone data
(define-private (expect-milestone (campaign-id uint) (milestone-id uint))
  (match (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id})
    milestone (ok milestone)
    ERR-NOT-FOUND))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant ERR-UNAUTHORIZED        (err u100))
(define-constant ERR-NOT-FOUND           (err u101))
(define-constant ERR-ALREADY-EXISTS      (err u102))
(define-constant ERR-INACTIVE            (err u103))
(define-constant ERR-TOO-LATE            (err u104))
(define-constant ERR-TOO-EARLY           (err u105))
(define-constant ERR-BAD-MILESTONE       (err u106))
(define-constant ERR-GOAL-NOT-MET        (err u107))
(define-constant ERR-ALREADY-APPROVED    (err u108))
(define-constant ERR-INSUFFICIENT        (err u109))
(define-constant ERR-NOT-CONTRIBUTOR     (err u110))
(define-constant ERR-PAUSED              (err u111))
(define-constant ERR-BAD-PARAMS          (err u112))
(define-constant ERR-DUPLICATE           (err u113))
(define-constant ERR-ALREADY-FINAL       (err u114))
(define-constant ERR-WINDOW              (err u115))
(define-constant ERR-NOT-ALLOWED         (err u116))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Admin / Operators / Pausing / Fees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-data-var admin principal tx-sender)
(define-map operators principal bool)
(define-data-var paused bool false)

(define-data-var platform-fee-bps uint u250)            ;; 2.50% fee on milestone withdrawals
(define-data-var fee-recipient principal tx-sender)

(define-private (only-admin) 
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) ERR-UNAUTHORIZED)
    (ok true)))
(define-private (only-op-or-admin)
  (begin
    (asserts! (or (is-eq tx-sender (var-get admin)) (default-to false (map-get? operators tx-sender))) ERR-UNAUTHORIZED)
    (ok true)))
(define-private (when-active) 
  (begin
    (asserts! (not (var-get paused)) ERR-PAUSED)
    (ok true)))



(define-public (set-operator (who principal) (flag bool))
  (begin
    (try! (only-admin))
    (map-set operators who flag) 
    (ok true)))

(define-public (set-paused (p bool))
  (begin
    (try! (only-op-or-admin))
    (var-set paused p)
    (ok p)))

(define-public (set-platform-fee (bps uint) (recipient principal))
  (begin
    (try! (only-op-or-admin))
    (asserts! (<= bps u10000) ERR-BAD-PARAMS)
    (var-set platform-fee-bps bps)
    (var-set fee-recipient recipient)
    (ok true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tokens (Badges)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-non-fungible-token backer-badge uint)
(define-data-var next-badge-id uint u1)
(define-map has-badge principal bool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Campaign Storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-data-var next-campaign-id uint u1)

(define-map campaigns
  uint
  {
    creator: principal,
    goal: uint,
    raised: uint,
    deadline: uint,
    current-milestone: uint,       ;; next milestone id to request
    total-milestones: uint,
    active: bool,
    arbiter: (optional principal), ;; optional dispute resolver
    vote-window: uint,             ;; blocks allowed to vote after request
    approve-threshold-bps: uint,   ;; % of voting power that must approve
    allowlist-enabled: bool,
    hard-cap: (optional uint)      ;; optional max total raise
  })

;; milestone definitions
(define-map milestones
  {campaign-id: uint, milestone-id: uint}
  {
    description: (buff 120),
    amount: uint,
    approved: bool,
    requested-height: (optional uint),  ;; set when creator requests approval
    finalized: bool                    ;; true after paid/failed/closed
  })

;; contributor accounting
(define-map contributions
  {campaign-id: uint, backer: principal}
  {
    amount: uint,
    refunded: uint                     ;; amount already refunded
  })

;; quick aggregates
(define-map campaign-stats
  uint
  {
    backers: uint,                     ;; count of distinct backers
    requested-total: uint,             ;; sum of milestone amounts requested so far
    withdrawn-total: uint              ;; actually paid out to creator (gross)
  })

;; voting by milestone (weighted by contribution at vote time)
(define-map votes
  {campaign-id: uint, milestone-id: uint, voter: principal}
  {
    support: bool,
    weight: uint                       ;; captured at first vote from voter
  })

(define-read-only (get-vote (key {campaign-id: uint, milestone-id: uint, voter: principal}))
  (map-get? votes key))

(define-private (sum-votes (campaign-id uint) (milestone-id uint) (support bool))
  (fold + 
    (map get-vote-weight-or-zero 
      (map unwrap-or-zero 
        (map get-vote (list {campaign-id: campaign-id, milestone-id: milestone-id, voter: tx-sender}))))
    u0))

(define-private (get-vote-weight-or-zero (vote-data {support: bool, weight: uint}))
  (if (get support vote-data)
      (get weight vote-data)
      u0))

(define-private (unwrap-or-zero (vote (optional {support: bool, weight: uint})))
  (default-to {support: false, weight: u0} vote))

(define-read-only (get-vote-weight (campaign-id uint) (milestone-id uint) (support bool))
  (sum-votes campaign-id milestone-id support))

;; allowlist (if enabled)
(define-map campaign-allowlist
  {campaign-id: uint, who: principal}
  bool

)

;; stretch goals
(define-map stretch-goals
  {campaign-id: uint, goal-id: uint}
  {
    target: uint,
    unlocked: bool,
    note: (buff 80)
  })

;; matching funds (sponsors match up to cap with a ratio numerator/denominator)
(define-map matching
  uint
  {
    sponsor: principal,
    ratio-num: uint,       ;; e.g., 1
    ratio-den: uint,       ;; e.g., 1 => 1:1
    cap: uint,             ;; maximum sponsor will match
    matched: uint          ;; how much already matched
  })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-private (pay (to principal) (amt uint))
  (if (> amt u0)
      (stx-transfer? amt (as-contract tx-sender) to)
      (ok true)))

(define-private (collect (from principal) (amt uint))
  (if (> amt u0) 
      (stx-transfer? amt from (as-contract tx-sender))
      (ok true)))



(define-private (update-stats (cid uint) (f (response {backers: uint, requested-total: uint, withdrawn-total: uint} uint)))
  (let ((st (match (map-get? campaign-stats cid)
              data data
              {backers: u0, requested-total: u0, withdrawn-total: u0})))
    (match f
      okv (begin (map-set campaign-stats cid okv) (ok true))
      err (ok false))))

(define-private (maybe-mint-badge (who principal))
  (if (not (default-to false (map-get? has-badge who)))
      (let ((bid (var-get next-badge-id)))
        (try! (nft-mint? backer-badge bid who))
        (var-set next-badge-id (+ bid u1))
        (map-set has-badge who true)
        (ok true))
      (ok false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create / Configure Campaign
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (create-campaign
  (goal uint)
  (deadline uint)
  (total-milestones uint)
  (arbiter (optional principal))
  (vote-window uint)                   ;; blocks
  (approve-threshold-bps uint)         ;; 0..10000
  (allowlist-enabled bool)
  (hard-cap (optional uint)))
  (begin
    (try! (when-active))
    (asserts! (> goal u0) ERR-BAD-PARAMS)
    (asserts! (> total-milestones u0) ERR-BAD-PARAMS)
    (asserts! (> vote-window u0) ERR-BAD-PARAMS)
    (asserts! (and (>= approve-threshold-bps u1) (<= approve-threshold-bps u10000)) ERR-BAD-PARAMS)
    (let ((id (var-get next-campaign-id)))
      (map-set campaigns id {
        creator: tx-sender,
        goal: goal,
        raised: u0,
        deadline: deadline,
        current-milestone: u1,
        total-milestones: total-milestones,
        active: true,
        arbiter: arbiter,
        vote-window: vote-window,
        approve-threshold-bps: approve-threshold-bps,
        allowlist-enabled: allowlist-enabled,
        hard-cap: hard-cap
      })
      (map-set campaign-stats id {backers: u0, requested-total: u0, withdrawn-total: u0})
      (var-set next-campaign-id (+ id u1))
      (ok id))))

(define-public (set-allowlist (campaign-id uint) (who principal) (allowed bool))
  (match (get-campaign-data campaign-id)
    campaign 
      (begin
        (asserts! (is-eq (get creator campaign) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get allowlist-enabled campaign) ERR-NOT-ALLOWED)
        (if allowed
            (map-set campaign-allowlist {campaign-id: campaign-id, who: who} true)
            (map-delete campaign-allowlist {campaign-id: campaign-id, who: who}))
        (ok true))
    err err))

(define-public (define-milestone (campaign-id uint) (milestone-id uint) (desc (buff 120)) (amount uint))
  (match (get-campaign-data campaign-id)
    campaign
      (begin
        (asserts! (is-eq (get creator campaign) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get active campaign) ERR-INACTIVE)
        (asserts! (> amount u0) ERR-BAD-PARAMS)
        (asserts! (<= milestone-id (get total-milestones campaign)) ERR-BAD-MILESTONE)
        (asserts! (is-none (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id})) ERR-ALREADY-EXISTS)
        (map-set milestones {campaign-id: campaign-id, milestone-id: milestone-id}
          { description: desc, amount: amount, approved: false, requested-height: none, finalized: false })
        (ok true))
    err err))

(define-public (set-stretch-goal (campaign-id uint) (goal-id uint) (target uint) (note (buff 80)))
  (let ((campaign-data (get-campaign-data campaign-id)))
    (match campaign-data
      campaign
        (begin
          (asserts! (is-eq (get creator campaign) tx-sender) ERR-UNAUTHORIZED)
          (asserts! (> target u0) ERR-BAD-PARAMS)
          (map-set stretch-goals {campaign-id: campaign-id, goal-id: goal-id} {target: target, unlocked: false, note: note})
          (ok true))
      err err)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contribute / Match / Refund / Cancel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-private (validate-contribution (campaign {active: bool, allowlist-enabled: bool, approve-threshold-bps: uint, arbiter: (optional principal), creator: principal, current-milestone: uint, deadline: uint, goal: uint, hard-cap: (optional uint), raised: uint, total-milestones: uint, vote-window: uint}) (amount uint) (campaign-id uint))
  (begin
    (asserts! (get active campaign) ERR-INACTIVE)
    (asserts! (<= stacks-block-height (get deadline campaign)) ERR-TOO-LATE)
    (asserts! (> amount u0) ERR-BAD-PARAMS)
    (if (get allowlist-enabled campaign)
        (asserts! (default-to false (map-get? campaign-allowlist {campaign-id: campaign-id, who: tx-sender})) ERR-NOT-ALLOWED)
        true)
    (if (is-some (get hard-cap campaign))
        (asserts! (<= (+ (get raised campaign) amount) (unwrap-panic (get hard-cap campaign))) ERR-INSUFFICIENT)
        true)
    (ok true)))

(define-public (contribute (campaign-id uint) (amount uint))
  (match (get-campaign-data campaign-id)
    campaign
      (match (validate-contribution campaign amount campaign-id)
        success
          (begin
            ;; collect funds first
            (try! (collect tx-sender amount))
            ;; mint badge if needed
            (try! (maybe-mint-badge tx-sender))
            ;; update contribution record
            (let ((prev (default-to {amount: u0, refunded: u0} (map-get? contributions {campaign-id: campaign-id, backer: tx-sender}))))
              (map-set contributions 
                {campaign-id: campaign-id, backer: tx-sender} 
                {amount: (+ (get amount prev) amount), refunded: (get refunded prev)})
              ;; update campaign total
              (map-set campaigns campaign-id (merge campaign {raised: (+ (get raised campaign) amount)}))
              ;; update backer count if first time
              (let ((is-first (is-eq (get amount prev) u0)))
                (if is-first
                    (let ((st (default-to {backers: u0, requested-total: u0, withdrawn-total: u0} (map-get? campaign-stats campaign-id))))
                      (map-set campaign-stats campaign-id (merge st {backers: (+ (get backers st) u1)})))
                    true)
                ;; try to apply matching if available
                (try! (apply-matching campaign-id amount))
                ;; all operations successful
                (ok true))))
        err (err err))
    err err))

(define-public (cancel-campaign (campaign-id uint))
  (match (get-campaign-data campaign-id)
    camp
      (begin 
        (asserts! (is-eq (get creator camp) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get active camp) ERR-INACTIVE)
        (map-set campaigns campaign-id (merge camp {active: false}))
        (ok true))
    err err))

(define-public (request-refund (campaign-id uint))
  (match (get-campaign-data campaign-id)
    camp
      (let ((contrib (map-get? contributions {campaign-id: campaign-id, backer: tx-sender})))
        (match contrib
          cd
            (let ((refundable
                    (if (or (not (get active camp))
                            (> stacks-block-height (get deadline camp)))
                        ;; if goal not met -> refund full available  
                        (if (< (get raised camp) (get goal camp))
                            (- (get amount cd) (get refunded cd))
                            u0)
                        u0)))
              (asserts! (> refundable u0) ERR-NOT-CONTRIBUTOR)
              (map-set contributions {campaign-id: campaign-id, backer: tx-sender} (merge cd {refunded: (+ (get refunded cd) refundable)}))
              (try! (pay tx-sender refundable))
              (ok refundable))
          ERR-NOT-CONTRIBUTOR))
    err err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching Funds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (set-matching (campaign-id uint) (ratio-num uint) (ratio-den uint) (cap uint))
  (match (get-campaign-data campaign-id)
    camp
      (begin
        (asserts! (is-eq (get creator camp) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (and (> ratio-num u0) (> ratio-den u0) (> cap u0)) ERR-BAD-PARAMS)
        (map-set matching campaign-id {sponsor: tx-sender, ratio-num: ratio-num, ratio-den: ratio-den, cap: cap, matched: u0})
        (ok true))
    err err))

;; Sponsor tops up matching pool (escrowed in contract)
(define-public (fund-matching (campaign-id uint) (amount uint))
  (let ((m (map-get? matching campaign-id)))
    (match m
      mm
        (begin 
          (try! (collect tx-sender amount))
          (ok true))
      ERR-NOT-FOUND)))

;; Internal helper: on contribution, try to match and credit campaign raised
;; (Kept simple: creator later withdraws via milestones as usual.)
(define-private (apply-matching (campaign-id uint) (contrib-amount uint))
  (let ((m (map-get? matching campaign-id)))
    (match m
      mm
        (let ((potential (/ (* contrib-amount (get ratio-num mm)) (get ratio-den mm)))
              (remaining (- (get cap mm) (get matched mm))))
          (let ((to-match (if (> potential remaining) remaining potential)))
            (if (> to-match u0)
                (begin
                  (map-set matching campaign-id (merge mm {matched: (+ (get matched mm) to-match)}))
                  (match (get-campaign-data campaign-id)
                    camp
                      (begin
                        (map-set campaigns campaign-id (merge camp {raised: (+ (get raised camp) to-match)}))
                        (ok to-match))
                    err err))
                (ok u0))))
      (ok u0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Milestone Approval (Backer Weighted Voting) + Arbiter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creator opens a milestone for approval (starts vote window)
(define-public (request-approval (campaign-id uint) (milestone-id uint))
  (match (get-campaign-data campaign-id)
    camp
      (begin
        (asserts! (is-eq (get creator camp) tx-sender) ERR-UNAUTHORIZED)
        (asserts! (get active camp) ERR-INACTIVE)
        (asserts! (is-eq (get current-milestone camp) milestone-id) ERR-BAD-MILESTONE)
        (let ((m (unwrap! (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id}) ERR-NOT-FOUND)))
          (asserts! (not (get finalized m)) ERR-ALREADY-FINAL)
          (asserts! (is-none (get requested-height m)) ERR-DUPLICATE)
          (map-set milestones {campaign-id: campaign-id, milestone-id: milestone-id}
            (merge m {requested-height: (some stacks-block-height)}))
          ;; store requested-total
          (let ((st (default-to {backers: u0, requested-total: u0, withdrawn-total: u0} (map-get? campaign-stats campaign-id))))
            (map-set campaign-stats campaign-id (merge st {requested-total: (+ (get requested-total st) (get amount m))})))
          (ok true)))
    err err))

;; Council-free model: backers vote within window. Weight = current (amount - refunded)


;; Council-free model: backers vote within window. Weight = current (amount - refunded)
(define-public (vote-milestone (campaign-id uint) (milestone-id uint) (support bool))
  (match (get-campaign-data campaign-id)
    camp
      (let ((m (unwrap! (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id}) ERR-NOT-FOUND)))
        (asserts! (is-some (get requested-height m)) ERR-WINDOW)
        (let ((opened (unwrap! (get requested-height m) ERR-WINDOW)))
          (asserts! (<= (- stacks-block-height opened) (get vote-window camp)) ERR-WINDOW))
        (let ((contrib (map-get? contributions {campaign-id: campaign-id, backer: tx-sender})))
          (match contrib
            cd
              (let ((weight (- (get amount cd) (get refunded cd))))
                (begin
                  (asserts! (> weight u0) ERR-NOT-CONTRIBUTOR)
                  (asserts! (is-none (map-get? votes {campaign-id: campaign-id, milestone-id: milestone-id, voter: tx-sender})) ERR-DUPLICATE)
                  (map-set votes {campaign-id: campaign-id, milestone-id: milestone-id, voter: tx-sender} {support: support, weight: weight})
                  (ok true)))
            ERR-NOT-CONTRIBUTOR)))
    err err))

(define-read-only (tally-votes (campaign-id uint) (milestone-id uint))
  (let (
    (c (map-get? campaigns campaign-id))
    (m (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id}))
  )
    (match c
      cc
        (match m
          mm
            (let (
              (yes (get-vote-weight campaign-id milestone-id true))
              (no (get-vote-weight campaign-id milestone-id false))
              (total (+ yes no))
              (approve-bps (if (> total u0) (/ (* yes u10000) total) u0))
            )
              (ok {
                yes: yes,
                no: no,
                total: total,
                approve-bps: approve-bps,
                threshold-bps: (get approve-threshold-bps cc)
              }))
          (err u0))
      (err u0))))

(define-private (proceed-withdraw (campaign-id uint) (milestone-id uint))
  (match (get-campaign-data campaign-id)
    camp
      (let ((m (unwrap! (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id}) ERR-NOT-FOUND)))
        (asserts! (not (get finalized m)) ERR-ALREADY-FINAL)
        (let (
          (amount (get amount m))
          (fee-bps (var-get platform-fee-bps))
          (fee (/ (* amount fee-bps) u10000))
          (to-creator (- amount fee))
          (creator (get creator camp)))
          ;; ensure campaign has enough raised not yet withdrawn
          (let (
            (stats (default-to {backers: u0, requested-total: u0, withdrawn-total: u0} (map-get? campaign-stats campaign-id)))
            (available (- (get raised camp) (get withdrawn-total stats))))
            (begin
              (asserts! (>= available amount) ERR-INSUFFICIENT)
              ;; mark milestone, increment current pointer
              (map-set milestones {campaign-id: campaign-id, milestone-id: milestone-id} (merge m {approved: true, finalized: true}))
              (map-set campaigns campaign-id (merge camp {current-milestone: (+ (get current-milestone camp) u1)}))
              ;; accounting
              (map-set campaign-stats campaign-id (merge stats {withdrawn-total: (+ (get withdrawn-total stats) amount)}))
              ;; payouts - check responses from both transfers
              (try! (pay (var-get fee-recipient) fee))
              (try! (pay creator to-creator))
              (ok {paid: to-creator, fee: fee})))))
    err err))

(define-private (reject-milestone (campaign-id uint) (milestone-id uint))
  (let ((m (unwrap! (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id}) ERR-NOT-FOUND)))
    (asserts! (not (get finalized m)) ERR-ALREADY-FINAL)
    (map-set milestones {campaign-id: campaign-id, milestone-id: milestone-id} (merge m {approved: false, finalized: true}))
    (ok true)))

;; Arbiter decision (optional)  callable by arbiter only, anytime after request
(define-public (arbiter-approve (campaign-id uint) (milestone-id uint) (approve bool))
  (match (get-campaign-data campaign-id)
    camp
      (let ((m (unwrap! (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id}) ERR-NOT-FOUND)))
        (let ((arb (get arbiter camp)))
          (asserts! (is-some arb) ERR-UNAUTHORIZED)
          (asserts! (is-eq tx-sender (unwrap-panic arb)) ERR-UNAUTHORIZED)
          (if approve
              (proceed-withdraw campaign-id milestone-id)
              (match (reject-milestone campaign-id milestone-id)
                success (ok {paid: u0, fee: u0})
                error (err error)))))
    err err))

;; Anyone can finalize after window: checks vote threshold and proceeds/ rejects.
(define-public (finalize-milestone (campaign-id uint) (milestone-id uint))
  (match (get-campaign-data campaign-id)
    camp
      (let ((milestone (unwrap! (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id}) ERR-NOT-FOUND)))
        (begin
          (asserts! (is-some (get requested-height milestone)) ERR-WINDOW)
          (let ((opened (unwrap! (get requested-height milestone) ERR-WINDOW)))
            (asserts! (> (- stacks-block-height opened) (get vote-window camp)) ERR-TOO-EARLY))
          (let ((tally (tally-votes campaign-id milestone-id)))
            (if (is-ok tally)
              (let ((vote-result (unwrap! tally (err u0))))
                (if (>= (get approve-bps vote-result) (get threshold-bps vote-result))
                  (proceed-withdraw campaign-id milestone-id)
                  (match (reject-milestone campaign-id milestone-id)
                    success (ok {paid: u0, fee: u0})
                    error (err error))))
              (err u0)))))
    err err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read-only Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns campaign-id))

(define-read-only (get-milestone (campaign-id uint) (milestone-id uint))
  (map-get? milestones {campaign-id: campaign-id, milestone-id: milestone-id}))

(define-read-only (get-contribution (campaign-id uint) (who principal))
  (map-get? contributions {campaign-id: campaign-id, backer: who}))

(define-read-only (get-stats (campaign-id uint))
  (map-get? campaign-stats campaign-id))

(define-read-only (get-stretch (campaign-id uint) (goal-id uint))
  (map-get? stretch-goals {campaign-id: campaign-id, goal-id: goal-id}))

(define-read-only (is-allowed (campaign-id uint) (who principal))
  (default-to false (map-get? campaign-allowlist {campaign-id: campaign-id, who: who})))

(define-read-only (badge-owned? (who principal))
  (default-to false (map-get? has-badge who)))

(define-read-only (refundable-amount (campaign-id uint) (who principal))
  (ok
    (match (map-get? campaigns campaign-id) campaign
      (match (map-get? contributions {campaign-id: campaign-id, backer: who}) contribution
        (if (or (not (get active campaign))
                (> stacks-block-height (get deadline campaign)))
            (if (< (get raised campaign) (get goal campaign))
                (- (get amount contribution) (get refunded contribution))
                u0)
            u0)
        u0)
      u0)))

(define-read-only (available-to-withdraw (campaign-id uint))
  (let ((c (map-get? campaigns campaign-id)))
    (match c
      cc
        (let ((stats (default-to {backers: u0, requested-total: u0, withdrawn-total: u0} (map-get? campaign-stats campaign-id))))
          (- (get raised cc) (get withdrawn-total stats)))
      u0)))
