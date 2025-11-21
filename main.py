import cmd
import json
from pathlib import Path
from itertools import islice
from typing import TypedDict, List

from dotenv import load_dotenv

from langchain_core.messages import AIMessage
from langchain_openai import OpenAIEmbeddings, ChatOpenAI
from langchain_community.document_loaders import JSONLoader
from langchain.tools import tool
from langchain.agents import create_agent
from langchain_chroma import Chroma

################################################################################

load_dotenv()

model = ChatOpenAI(model="gpt-4o-mini")
embeddings = OpenAIEmbeddings(model="text-embedding-3-small")
vector_store = Chroma(
    collection_name="agda-signatures",
    embedding_function=embeddings,
    persist_directory="./chroma_db"
)

################################################################################
# Load the agda standard library and store the document chunks in the vector store if not already present

def batched(iterable, n):
    """Yield lists of size <= n from iterable."""
    it = iter(iterable)
    while True:
        chunk = list(islice(it, n))
        if not chunk:
            break
        yield chunk

def metadata_func(record: dict, metadata: dict) -> dict:
    metadata["name"] = record["metadata"]["name"]
    metadata["module"] = record["metadata"]["module_name"]
    metadata["defined_in"] = record["metadata"]["top_level_module_name"]
    metadata["type"] = record["metadata"]["signature"]
    metadata["definition_kind"] = record["metadata"]["kind"]
    return metadata

def build_index_if_empty():
    count = vector_store._collection.count()
    if count > 0:
        print(f"Vector store already has {count} embeddings, skip indexing.")
        return

    print("Vector store is empty. Building index...")

    print("Loading signatures...")

    documents = []
    for path in Path("./signatures").glob("**/*.jsonl"):
        loader = JSONLoader(
            file_path=path,
            jq_schema=".",
            content_key="page_content",
            json_lines=True,
            metadata_func=metadata_func,
        )
        documents.extend(loader.load())

    print(f"Loaded {len(documents)} signatures")

    print("Storing the documents...")

    document_ids = []
    BATCH_SIZE = 500

    for i, batch in enumerate(batched(documents, BATCH_SIZE), start=1):
        ids = vector_store.add_documents(documents=batch)
        document_ids.extend(ids)
        print(f"Batch {i}: stored {len(ids)} documents (total {len(document_ids)})")

    print(f"Stored {len(document_ids)} documents in total")

build_index_if_empty()

################################################################################
# RAG Agent

class Definition(TypedDict):
    name: str
    module: str
    defined_in: str
    type: str
    definition_kind: str
    page_content: str

@tool
def retrieve_context(query: str) -> str:
    """Retrieve information from the Agda libraries to help answer a query about Agda code."""
    retrieved_docs = vector_store.similarity_search(query, k=5)

    results: List[Definition] = []
    for doc in retrieved_docs:
        results.append({
            "name": doc.metadata["name"],
            "module": doc.metadata["module"],
            "defined_in": doc.metadata["defined_in"],
            "type": doc.metadata["type"],
            "definition_kind": doc.metadata["definition_kind"],
            "page_content": doc.page_content,
        })

    return json.dumps(results, ensure_ascii=False, indent=2)

tools = [retrieve_context]
prompt = prompt = """
You are an assistant that searches the Agda standard library source code.

You have access to a tool called `retrieve_context`.
- This tool takes a natural language or type-based query as input.
- It returns a JSON array of objects.
- Each object has (at least) the following fields:
  - "name": the name of the item (definition, data, record, etc.)
  - "type": the type signature or definition as a string
  - "module": the fully qualified module name
  - "defined_in": the top-level module name that the item is defined in
  - "definition_kind": the kind of the item (function, datatype, record, etc.)
  - "page_content": the raw stored text (may contain extra context)

When a user asks about an item in the Agda standard library:
1. ALWAYS call `retrieve_context` first with an appropriate query.
2. Read the JSON returned by the tool.
3. Use ONLY the fields from that JSON when constructing your answer.

Output format:
- For each relevant item, output in this exact format:

Name: <definition name>
Type: <type signature>
Module: <module name>
Defined in: <top-level module name>
Definition kind: <definition kind>

- Separate multiple items with a blank line.
- Do NOT add any other commentary or explanation.

Important constraints:
- Do NOT invent module names, definition names, or type signatures.
- If `retrieve_context` returns an empty array, say:
  "No matching definitions were found in the Agda standard library."
  and do not fabricate any definitions.
- If the user asks about something outside the Agda standard library, explain that your knowledge is restricted to the indexed library and that no matching definition was found.
"""

agent = create_agent(model, tools, system_prompt=prompt)

################################################################################
# Try the agent

class LibrarySearchAgent(cmd.Cmd):
    intro = "Welcome to the Agda standard library search agent. Type 'help' or '?' for available commands."
    prompt = ">> "

    def do_exit(self, arg):
        """Exit the agent."""
        print("Bye!")
        return True

    def default(self, line):
        """Search the Agda standard library for the given query."""
        response = agent.invoke({"messages": [{"role": "user", "content": line}]})
        for chunk in response['messages']:
            if isinstance(chunk, AIMessage):
                print(chunk.content)

LibrarySearchAgent().cmdloop()
