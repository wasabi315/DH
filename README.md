# Library Search Agent for Agda

A search agent for the Agda standard library. This project uses RAG (Retrieval-Augmented Generation) technology to efficiently search for definitions and type signatures in the Agda standard library.

This project was created as an assignment for the course "Collaboration across STEM and Liberal Arts: AI design concept and technology that supports digital humanities".

## Features

- **Vector Search**: Stores the Agda standard library signatures in a vector database and enables semantic similarity-based search
- **Interactive Interface**: Interactively search the Agda standard library from the command line
- **RAG Agent**: Uses a LangChain-based RAG agent to search for relevant definitions from natural language queries

## Setup

### Requirements

- Python 3.13 or higher
- OpenAI API key (set as an environment variable)

### Installation

1. Clone the repository:

```bash
git clone <repository-url>
cd DH
```

2. Install dependencies:

```bash
uv sync
```

3. Set up environment variables:

Create a `.env` file and add the following:

```text
OPENAI_API_KEY=your_api_key_here
```

## Usage

Running the project will launch an interactive command-line interface:

```bash
python main.py
```

### Examples

Once the agent is running, you can input queries about the Agda standard library:

```text
>> addition is commutative
>> List permutation relation
>> Permutation (reverse xs) xs
```

Type `exit` to quit.

## Tech Stack

- **LangChain**: RAG agent construction
- **Chroma**: Vector database
- **OpenAI**: Embedding model (text-embedding-3-small) and LLM (gpt-4o-mini)
- **Python**: Implementation language (RAG Agent)
- **Haskell**: Implementation language (Pre-processing)

## How It Works

1. **Signature Extraction**: Extracts signatures from the Agda standard library source code into JSONL files in the `signatures/` directory
2. **Data Loading**: Loads Agda signatures from the JSONL files
3. **Vectorization**: Vectorizes each signature using OpenAI's embedding model
4. **Storage**: Stores them in the Chroma vector database
5. **Search**: Vectorizes user queries and performs similarity search
6. **Response Generation**: LLM generates answers based on search results

## License

See the LICENSE file for license information.
