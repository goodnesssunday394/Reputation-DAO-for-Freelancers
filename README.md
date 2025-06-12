# 🌟 Reputation DAO for Freelancers

A decentralized reputation system for freelancers built on Stacks blockchain using Clarity smart contracts.

## 🎯 Features

- 👤 Freelancer profile registration
- ⭐ Token-backed rating system
- 💰 Stake-to-rate mechanism
- 🏆 Premium status for high-rated freelancers
- 🔒 Tamper-proof reputation tracking

## 🚀 Getting Started

### Prerequisites

- Clarinet
- Stacks wallet

### Contract Functions

1. **Register as Freelancer**
```clarity
(contract-call? .reputation-dao register-freelancer)
```

2. **Submit Rating**
```clarity
(contract-call? .reputation-dao submit-rating freelancer-id rating stake-amount)
```

3. **Check Freelancer Details**
```clarity
(contract-call? .reputation-dao get-freelancer-details freelancer-id)
```

4. **Verify Premium Status**
```clarity
(contract-call? .reputation-dao is-premium-freelancer freelancer-id)
```

## 💡 How it Works

1. Freelancers register to get a unique profile NFT
2. Clients stake tokens to submit ratings
3. Ratings affect the freelancer's average score
4. Freelancers with 80+ rating achieve premium status
5. Staking mechanism prevents spam and ensures honest feedback

## 🔑 Key Parameters

- Minimum stake amount: 100 tokens
- Premium threshold: 80/100 rating
- Rating range: 1-100
```
