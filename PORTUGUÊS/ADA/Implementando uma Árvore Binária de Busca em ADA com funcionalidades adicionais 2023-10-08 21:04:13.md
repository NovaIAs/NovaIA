Claro! Aqui está um código complexo em ADA que envolve a implementação de uma estrutura de dados de Árvore Binária de Busca (ABB) com funcionalidades adicionais. Vou explicar o código passo a passo para que você possa entendê-lo melhor:

```ada
-- Definição do tipo de dado para os nós da árvore
type Node is record
    Value : Integer;
    Left  : access Node := null;
    Right : access Node := null;
end record;

-- Definição do tipo de dado para a árvore binária de busca
type BinarySearchTree is access Node;

-- Função para criar um novo nó
function NewNode(Item : Integer) return BinarySearchTree is
    New_Node : BinarySearchTree := new Node;
begin
    New_Node.Value := Item;
    return New_Node;
end NewNode;

-- Procedimento para inserir um nó na árvore
procedure Insert(Node_Ptr : in out BinarySearchTree; Item : Integer) is
begin
    if Node_Ptr = null then
        Node_Ptr := NewNode(Item);
    else
        if Item < Node_Ptr.Value then
            Insert(Node_Ptr.Left, Item);
        elsif Item > Node_Ptr.Value then
            Insert(Node_Ptr.Right, Item);
        end if;
    end if;
end Insert;

-- Função para buscar um nó na árvore
function Search(Node_Ptr : BinarySearchTree; Item : Integer) return Boolean is
begin
    if Node_Ptr = null then
        return False;
    elsif Item = Node_Ptr.Value then
        return True;
    elsif Item < Node_Ptr.Value then
        return Search(Node_Ptr.Left, Item);
    else
        return Search(Node_Ptr.Right, Item);
    end if;
end Search;

-- Procedimento para imprimir a árvore em ordem
procedure PrintInOrder(Node_Ptr : BinarySearchTree) is
begin
    if Node_Ptr /= null then
        PrintInOrder(Node_Ptr.Left);
        Put(Node_Ptr.Value'Image);
        PrintInOrder(Node_Ptr.Right);
    end if;
end PrintInOrder;

-- Procedimento para excluir um nó da árvore
procedure Delete(Node_Ptr : in out BinarySearchTree; Item : Integer) is
    Successor : BinarySearchTree;
begin
    if Node_Ptr = null then
        return;
    elsif Item < Node_Ptr.Value then
        Delete(Node_Ptr.Left, Item);
    elsif Item > Node_Ptr.Value then
        Delete(Node_Ptr.Right, Item);
    else
        if Node_Ptr.Left = null then
            Node_Ptr := Node_Ptr.Right;
        elsif Node_Ptr.Right = null then
            Node_Ptr := Node_Ptr.Left;
        else
            Successor := Node_Ptr.Right;
            while Successor.Left /= null loop
                Successor := Successor.Left;
            end loop;
            Node_Ptr.Value := Successor.Value;
            Delete(Node_Ptr.Right, Successor.Value);
        end if;
    end if;
end Delete;
```

Este código em ADA implementa uma Árvore Binária de Busca (ABB) com as seguintes funcionalidades:

1. Criação de um novo nó: A função `NewNode` cria um novo nó da árvore com um determinado valor.
2. Inserção de um nó: O procedimento `Insert` insere um novo nó na árvore mantendo a propriedade de ordenação da ABB.
3. Busca de um nó: A função `Search` busca um nó na árvore com um determinado valor e retorna verdadeiro se o nó for encontrado.
4. Impressão em ordem: O procedimento `PrintInOrder` imprime os nós da árvore em ordem crescente.
5. Exclusão de um nó: O procedimento `Delete` remove um nó da árvore de forma a manter a propriedade de ordenação da ABB.

Espero que este código complexo em ADA ajude você a entender melhor a implementação de uma Árvore Binária de Busca!