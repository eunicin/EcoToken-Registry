# EcoToken-Registry

## Overview
EcoToken-Registry is a decentralized registry for environmental asset tokenization. It provides a mechanism to issue, transfer, and retire eco-assets as NFTs, ensuring verifiable tracking of carbon offset claims.

## Features
- **Issuance:** Authorized issuance of individual or grouped environmental assets.
- **Provenance Tracking:** Links each asset to verifiable data through URI references.
- **Secure Custody:** Tracks asset ownership throughout its lifecycle.
- **Permanent Retirement:** Prevents reuse of claimed offsets.
- **Verification Mechanism:** Ensures data integrity with validation controls.

## Security Measures
- **Admin-gated issuance** to maintain market integrity.
- **Retirement safeguards** to prevent double counting.

## Use Cases
EcoToken-Registry is designed for voluntary carbon market registries and organizations tracking environmental impact claims transparently.

## Prerequisites
To interact with the registry, you need:
- A Clarity-compatible blockchain (Stacks blockchain recommended)
- Clarity smart contract execution environment
- Basic knowledge of smart contracts and NFT tokenization

## Installation
Clone the repository:
```sh
git clone https://github.com/your-username/EcoToken-Registry.git
cd EcoToken-Registry
```

## Smart Contract Structure
- **eco-asset:** Defines the non-fungible token (NFT) standard for eco-credits.
- **registry-counter:** Tracks the latest registered asset ID.
- **asset-verification-data:** Maps asset IDs to verification details.
- **retired-assets:** Records permanently retired assets.

## Key Functions
### 1. **Register an Eco Asset**
```clarity
(define-public (register-eco-asset (verification-data (string-ascii 256))))
```
Registers a single environmental asset with verifiable data.

### 2. **Group Register Multiple Assets**
```clarity
(define-public (group-register-eco-assets (data-list (list 50 (string-ascii 256)))))
```
Allows batch registration of up to 50 assets.

### 3. **Retire an Asset Permanently**
```clarity
(define-public (retire-eco-asset (asset-id uint)))
```
Retires an asset, preventing future transfers.

### 4. **Verify Asset Status**
```clarity
(define-public (verify-asset-status (asset-id uint)))
```
Checks if an asset is valid and not retired.

## Testing
Run smart contract tests using the Clarity CLI:
```sh
clarity test
```

## Contribution
Contributions are welcome! Please open an issue or submit a pull request.

## License
This project is licensed under the MIT License.
