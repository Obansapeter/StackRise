# StackRise - Decentralized Milestone-Based Crowdfunding

![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![Platform: Stacks](https://img.shields.io/badge/Platform-Stacks-purple.svg)
![Smart Contract: Clarity](https://img.shields.io/badge/Smart%20Contract-Clarity-orange.svg)

A comprehensive decentralized crowdfunding platform built on Stacks blockchain with milestone-based funding releases, backer voting, and advanced features for Web3 projects.

## Features

### Core Functionality
- 🏗️ **Milestone-Based Funding**: Creators define multiple milestones with descriptions and funding amounts
- 💰 **Weighted Voting**: Backers vote on milestone completions with voting power based on contribution
- 🔒 **Secure Fund Management**: Automatic escrow and controlled release of funds
- 🎯 **Flexible Goals**: Support for both minimum funding goals and optional hard caps

### Advanced Features
- ⚖️ **Optional Arbiter**: Dispute resolution system for milestone approvals
- 🤝 **Matching Funds**: Sponsor matching with configurable ratios and caps
- 🏅 **NFT Badges**: Automatic minting of contributor badges
- 📋 **Allowlisting**: Optional qualified backer verification
- 💸 **Platform Fees**: Configurable fee structure on successful milestone completions
- 🔄 **Automatic Refunds**: Built-in refund mechanism if campaign fails

## Contract Functions

### Campaign Creation & Configuration
```clarity
(create-campaign (goal uint) (deadline uint) (total-milestones uint) ...)
(define-milestone (campaign-id uint) (milestone-id uint) (desc (buff 120)) (amount uint))
(set-stretch-goal (campaign-id uint) (goal-id uint) (target uint) (note (buff 80)))
```

### Backer Interactions
```clarity
(contribute (campaign-id uint) (amount uint))
(vote-milestone (campaign-id uint) (milestone-id uint) (support bool))
(request-refund (campaign-id uint))
```

### Milestone Management
```clarity
(request-approval (campaign-id uint) (milestone-id uint))
(finalize-milestone (campaign-id uint) (milestone-id uint))
```

## Security Features

- Role-based access controls
- Input validation and error handling
- Pausable functionality
- Safe math operations
- Milestone approval thresholds
- Voting windows
- Contribution caps

## Getting Started

1. Deploy contract to Stacks blockchain
2. Configure admin and platform settings
3. Create campaign with milestones
4. Set optional parameters (arbiter, allowlist, etc.)
5. Open for contributions

## Usage Example

```clarity
;; Create new campaign
(contract-call? .stackrise create-campaign
    u100000000 ;; Goal: 100 STX
    u720 ;; Deadline: 720 blocks
    u3 ;; Total milestones
    none ;; No arbiter
    u144 ;; 24hr voting window
    u6000 ;; 60% approval threshold
    false ;; No allowlist
    none) ;; No hard cap
```

## Development

Requirements:
- Clarity CLI
- Stacks blockchain local development setup
- Clarity VS Code extension (recommended)

## Testing

Comprehensive test suite included covering:
- Campaign lifecycle
- Milestone voting
- Fund management
- Security controls
- Edge cases
