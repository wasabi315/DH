import cmd
from pathlib import Path
from itertools import islice

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

if not Path("./chroma_db").exists():

    print("Loading signatures...")

    documents = []
    for path in Path("./signatures").glob("**/*.jsonl"):
        loader = JSONLoader(
            file_path=path,
            jq_schema=".",
            content_key="page_content",
            json_lines=True,
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

################################################################################
# RAG Agent

@tool
def retrieve_context(query: str) -> str:
    """Retrieve information from the Agda standard library to help answer a query about Agda code.

    Args:
        query: The search query about Agda standard library items (e.g., "List map", "Maybe type", etc.)

    Returns:
        A formatted string containing information about relevant definitions in the loaded Agda library.
    """
    # Increase k to get more relevant results
    retrieved_docs = vector_store.similarity_search(query, k=5)
    serialized = "\n\n".join(
        (f"Source: {doc.metadata.get('source', 'unknown')}\nContent:\n{doc.page_content}")
        for doc in retrieved_docs
    )
    return serialized

tools = [retrieve_context]
prompt = (
    "You are a helpful assistant that searches the Agda standard library source code."
    "When a user asks about an item (like 'List map'), use the retrieve_context tool to find relevant definitions."
    "You have access via the retrieve_context tool to a vector store containing information about the Agda library to search for relevant definitions."
    "There may be multiple relevant definitions for the same query, so provide all relevant definitions."
    "Only return the following information, no other text:\n"
    "1. The module name where the item is defined\n"
    "2. The name of the item\n"
    "3. The type signature or definition of the item\n"
    "Be precise. Never make up information."
)
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
