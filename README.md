# Quantum Error Correction Pipeline (QEC)

## Overview

The **Quantum Error Correction Pipeline (QEC)** is a modular, compositional framework implemented in Haskell that leverages functorial mappings, proof normalization, and algebraic error correction techniques. The pipeline is designed to be **verifiable, scalable, and extensible**, making it suitable for **advanced quantum computing applications**.

### Features

- ✅ **Compositional Design** – Each module transforms quantum state data in a structured manner.  
- ✅ **Functorial Error Correction** – Ensures transformations preserve quantum properties.  
- ✅ **Verifiable Outputs** – Each stage of the pipeline produces intermediate results for validation.  
- ✅ **Extensible Framework** – New modules can be integrated seamlessly.  

---

## Installation

### Prerequisites

Ensure you have the following dependencies installed:

- [Haskell Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- Cabal (installed with Stack)
- A LaTeX distribution (optional, for compiling documentation)

### Clone and Build

To set up the project, follow these steps:

1. Clone the repository:

   ```sh
   git clone https://github.com/MagnetonIO/proof_as_code_qec.git
   cd proof_as_code_qec
   ```

2. Build the project:

   ```sh
   stack build
   ```

3. Run tests (if applicable):

   ```sh
   stack test
   ```

---

## Running the QEC Pipeline

The **CLI** allows you to run the entire error correction process sequentially.

### Run with Default Configuration

```sh
stack run
```

### Run Specific Stages

You can execute individual stages manually for debugging:

```sh
stack ghci
> runPenaltyTerms "Initial QEC state"
> runFunctorialMapping "Previous Output"
> runProofNormalization "Previous Output"
> runBraiding "Previous Output"
> runErrorCorrection "Previous Output"
> runTypesAndInvariants "Previous Output"
```

---

## Command-line Interface (CLI)

A command-line interface is provided for flexibility:

```sh
stack exec qec-cli -- --help
```

### Available Commands

```sh
stack exec qec-cli -- --run-all       # Run the full pipeline
stack exec qec-cli -- --stage penalty # Run only the penalty terms module
stack exec qec-cli -- --stage braid   # Run only the braiding module
stack exec qec-cli -- --verbose       # Run with detailed output
```

---

## Module Overview

| Module                  | Description |
|-------------------------|-------------|
| `PenaltyTerms.hs`       | Introduces penalty-based transformations to regulate the error landscape. |
| `FunctorialMapping.hs`  | Applies category-theoretic mappings to preserve compositional properties. |
| `ProofNormalization.hs` | Normalizes proofs to simplify error-correcting logic. |
| `Braiding.hs`           | Implements topological transformations to enhance fault tolerance. |
| `ErrorCorrection.hs`    | Performs the final quantum error correction based on previous transformations. |
| `TypesAndInvariants.hs` | Verifies that the corrected state satisfies type-theoretic invariants. |

---

## Development

### Code Formatting

Use `ormolu` for consistent Haskell formatting:

```sh
stack install ormolu
ormolu -i src/*.hs
```

### Debugging

Run in GHCi for interactive debugging:

```sh
stack ghci
```

Enable verbose logging:

```sh
stack exec qec-cli -- --verbose
```

---

## Documentation

The **ArXiv-style documentation** provides an in-depth explanation of the pipeline:

- **[Paper: Functorial Quantum Error Correction](docs/qec_pipeline.pdf)**
- **[Paper: Proof Normalization](docs/proof_normalization.pdf)**
- **[Paper: Braiding and Error Correction](docs/braiding.pdf)**

To compile LaTeX documentation manually:

```sh
cd docs
pdflatex qec_pipeline.tex
```

---

## Contributing

1. Fork the repository.

2. Create a feature branch:

   ```sh
   git checkout -b feature-new-module
   ```

3. Commit changes:

   ```sh
   git commit -am 'Add new error correction module'
   ```

4. Push to the branch:

   ```sh
   git push origin feature-new-module
   ```

5. Open a pull request.

---

## License

This project is licensed under the **MIT License**.

---

## Contact

For questions or collaborations, reach out to:

📧 Email: [info@magnetonlabs.com](mailto:info@magnetonlabs.com)  
🔬 Research: [Magneton Labs](https://magnetonlabs.com)

---

🚀 **Run the QEC Pipeline today and enhance quantum computing reliability!**
