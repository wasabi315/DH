import cmd

from langchain_core.messages import AIMessage
from langchain_openai import OpenAIEmbeddings, ChatOpenAI
from langchain_core.vectorstores import InMemoryVectorStore
from langchain_community.document_loaders.generic import GenericLoader
from langchain_community.document_loaders.parsers.txt import TextParser
from langchain_text_splitters import RecursiveCharacterTextSplitter, Language
from langchain.tools import tool
from langchain.agents import create_agent

model = ChatOpenAI(model="gpt-4o-mini")
embeddings = OpenAIEmbeddings(model="text-embedding-3-small")
vector_store = InMemoryVectorStore(embeddings)

################################################################################
# Load the agda standard library

print("Loading the agda standard library...")

loader = GenericLoader.from_filesystem(
    "./agda-stdlib/src",
    glob="**/*",
    suffixes=[".agda"],
    parser=TextParser()
)

print("Loaded the agda standard library.")

################################################################################
# Split the loaded source code into chunks

print("Splitting the loaded source code into chunks...")

# Load all documents first (lazy_load() returns an iterator, so we need to materialize it)
print("Loading documents from filesystem...")

agda_splitter = RecursiveCharacterTextSplitter.from_language(
    # Agda's syntax is similar to Haskell's, so we use Haskell's language.
    Language.HASKELL,
    chunk_size=1200,
    chunk_overlap=200,
)

agda_chunks = agda_splitter.split_documents(loader.lazy_load())

print(f"Split the loaded source code into {len(agda_chunks)} chunks.")

################################################################################
# Store the document chunks

print("Storing the document chunks...")

# Store all chunks, not just the first 10
# For large datasets, you might want to batch this
document_ids = vector_store.add_documents(documents=agda_chunks)

print(f"Stored {len(document_ids)} document chunks.")

################################################################################
# RAG Agent

@tool
def retrieve_context(query: str) -> str:
    """Retrieve information from the Agda standard library to help answer a query about Agda code.

    Args:
        query: The search query about Agda standard library items (e.g., "List map", "Maybe type", etc.)

    Returns:
        A formatted string containing relevant source code snippets with their metadata.
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
    "You are a helpful assistant that searches the Agda standard library source code. "
    "When a user asks about an item (like 'List map'), use the retrieve_context tool to find relevant definitions."
    "Some may query with natural language, while others may query with the type signature of the item."
    "If the query is a type signature, you should try to find the item that matches the type signature."
    "If the query is a natural language query, you should try to find the item that matches the natural language query."
    "There may be multiple relevant definitions for the same query, so provide all relevant definitions."
    "Analyze the retrieved source code and provide the following information:\n"
    "1. The module name where the item is defined\n"
    "2. The name of the item\n"
    "3. The type signature or definition of the item\n"
    "4. Any relevant context from the source code\n\n"
    "Be precise and cite the source file."
    "Only return the information, no other text."
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

if __name__ == "__main__":
    LibrarySearchAgent().cmdloop()
