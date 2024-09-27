import ply.lex as lex
import ply.yacc as yacc

# ----------- Analisador Léxico -----------

# Lista de tokens
tokens = [
    'ID', 'NUMBER_INT', 'NUMBER_FLOAT', 'LBRACE', 'RBRACE','HASH', 'LBRACK', 'RBRACK', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'EQUALS', 'EQ', 'NEQ', 'LT', 'GT', 'LE', 'GE', 'LPAREN', 'RPAREN', 'AND', 'OR', 'NOT', 'NEG', 'SEMICOLON', 'DOT'
]

# Definindo os padrões para os tokens
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_HASH = r'\#'
t_LBRACK = r'\['
t_RBRACK = r'\]'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQ = r'=='
t_NEQ = r'!='
t_LT = r'<'
t_LE = r'<='
t_GT = r'>'
t_GE = r'>='
t_EQUALS = r'='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_AND = r'\&\&' 
t_OR = r'\|\|' 
t_NOT = '!'
t_NEG = r'-'
t_SEMICOLON = ';'
t_DOT = '\.'


# Lista de palavras reservadas
reserved = {
    'int': 'INT',
    'char': 'CHAR',
    'bool': 'BOOL',
    'record': 'RECORD',
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'break': 'BREAK',
    'print': 'PRINT',
    'read': 'READ',
    'true': 'TRUE',
    'false': 'FALSE'
}

tokens = tokens + list(reserved.values())

# Classe para a tabela de simbolos,
class SymbolTable:
    def __init__(self):
        self.symbols = {}

    # Inserir um novo símbolo na tabela
    def insert(self, name, var_type):
        if name not in self.symbols:
            self.symbols[name] = var_type
        return self.symbols[name]

    # Buscar se o símbolo existe na tabela
    def lookup(self, name):
        return self.symbols.get(name)

    def __repr__(self):
        return ', '.join(f'{name}: {var_type}' for name, var_type in self.symbols.items())
    
# Criando a tabela de símbolos e inserindo palavras reservadas
symbol_table = SymbolTable()
for word, symbol_type in reserved.items():
    symbol_table.insert(word, symbol_type)

# Funções do analisador léxico, resposaveis por indentificar e processar os tokens do código de entrada

# Definindo identificadores
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = symbol_table.lookup(t.value)
    if t.type is None:
        symbol_table.insert(t.value, tokens[0])
        t.type = tokens[0]
    return t

# Definindo números inteiros e floats
def t_NUMBER_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_NUMBER_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Definindo ignorância de espaços em branco
t_ignore = ' \t'

# Definindo nova linha
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Tratamento de erros
def t_error(t):
    print(f"Caractere inválido: {t.value[0]}")
    t.lexer.skip(1)

# Construir o analisador léxico
lexer = lex.lex()

# Função de teste para o lexer
def test_lexer(data):
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)

# ---------- Analisador Sintático----------

# Definição da gramática 
def p_program(p):
    '''program : block'''
    p[0] = p[1]

def p_block(p):
    '''block : LBRACE declaration statements RBRACE'''
    p[0] = (p[2], p[3])

#como pode ser 0 ou mais
def p_declaration(p):
    '''declaration : type ID SEMICOLON declaration
                   | empty'''
    if len(p) == 5:
        p[0] = (p[1], p[2], p[4])
    else:
        p[0] = None
        
def p_type(p):
    ''' type : basic dimensions
             | basic HASH
             | basic
             | RECORD LBRACE declaration RBRACE'''
    if len(p) == 5:
        p[0] = ('record', p[3])
    elif len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = p[1]

def p_dimensions(p):
    '''dimensions : LBRACK NUMBER_INT RBRACK dimensions
                  | LBRACK NUMBER_INT RBRACK'''
    if len(p) == 5:
        p[0] = (p[2], p[4])
    else:
        p[0] = p[2]

def p_basic(p):
    '''basic : INT
             | CHAR
             | BOOL'''
    p[0] = p[1]

def p_statements(p):
    '''statements : statement statements
                  | empty'''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = None

def p_statement(p):
    '''statement : local EQUALS boolean SEMICOLON
                 | conditional
                 | while_loop
                 | do_while_loop
                 | BREAK SEMICOLON
                 | print
                 | read
                 | block'''
    if len(p) == 5:  
        p[0] = ('=', p[1], p[3]) 
    elif len(p) == 2:  
        p[0] = p[1]  
    elif len(p) == 4: 
        p[0] = ('break') 
    else:
        print("Erro de sintaxe em statement")

def p_print(p):
    '''print : PRINT LPAREN boolean RPAREN SEMICOLON'''
    p[0] = ('print', p[3]) 

def p_read(p):
    '''read : READ LPAREN local RPAREN SEMICOLON'''
    p[0] = ('read', p[3]) 

def p_conditional(p):
    '''conditional : IF LPAREN boolean RPAREN statement conditional_aux'''
    p[0] = ('if', p[2], p[4], p[5])

def p_conditional_aux(p):
    '''conditional_aux : ELSE statement
                       | empty'''
    if len(p) == 3:
        p[0] = ('else', p[3])
    else:
        p[0] = None

def p_while_loop(p):
    '''while_loop : WHILE LPAREN boolean RPAREN statement'''
    p[0] = ('while', p[3], p[5])

def p_do_while_loop(p):
    '''do_while_loop : DO statement WHILE LPAREN boolean RPAREN SEMICOLON'''
    p[0] = ('do', p[2], 'while', p[5])

def p_local(p):
    '''local : ID
             | HASH ID
             | ID local_aux
             | HASH ID local_aux'''
    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    elif len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = p[1]

def p_local_aux(p):
    '''local_aux : offset
                 | field'''
    p[0] = p[1]

def p_offset(p):
    '''offset : LBRACK boolean RBRACK offset
              | empty'''
    if len(p) == 5:
        p[0] = (p[2], p[4])
    else:
        p[0] = None  
        
def p_field(p):
    '''field : DOT factor field
             | DOT factor'''
    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    else:
        p[0] = p[2]

def p_boolean(p):
    '''boolean : join
               | boolean OR join'''
    if len(p) == 2:
        p[0] = p[1]
    else: 
        p[0] = ('or', p[1], p[3])

def p_join(p):
    '''join : equality
            | join AND equality '''
    if len(p) == 2:
        p[0] = p[1]
    else: 
        p[0] = ('and', p[1], p[3])

def p_equality(p):
    '''equality : relational 
                | equality EQ relational
                | equality NEQ relational'''
    if len(p) == 2:
        p[0] = p[1]
    else: 
        p[0] = (p[2], p[1], p[3]) 

def p_relational(p):
    '''relational : expression 
                  | relational_operator expression'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = (p[1], p[2])

def p_relational_operator(p):
    '''relational_operator : LT
                           | LE
                           | GT
                           | GE'''
    p[0] = p[1]

def p_expression(p):
    '''expression : term 
                | expression expression_operator term'''
    if len(p) == 2:
        p[0] = p[1]
    else: 
        p[0] = (p[2], p[1], p[3]) 

def p_expression_operator(p):
    '''expression_operator : PLUS
                           | MINUS'''
    p[0] = p[1]

def p_term(p):
    '''term : unary
            | term TIMES unary
            | term DIVIDE unary'''
    if len(p) == 2:
        p[0] = p[1]
    else: 
        p[0] = (p[2], p[1], p[3])

def p_unary(p):
    '''unary : NOT unary
             | NEG unary
             | factor'''
    if len(p) == 3:
        p[0] = (p[1], p[2])
    else:
        p[0] = p[1]

def p_factor(p):
    '''factor : LPAREN boolean RPAREN
              | local
              | NUMBER_INT
              | NUMBER_FLOAT
              | FALSE
              | TRUE'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

def p_empty(p):
    '''empty :'''
    p[0] = None

# Tratamento de erros de sintaxe
def p_error(p):
    print(f"Erro de sintaxe em '{p.value}'" if p else "Erro de sintaxe no final da entrada")

# Construir o parser
parser = yacc.yacc()

# Função de teste para o parser
def test_parser(data):
    result = parser.parse(data)
    return result

# def test_lexer(data):
#     lexer.input(data)
#     while True:
#         tok = lexer.token()
#         if not tok:
#             break
#         print(tok)

# Função para realizar a entrada de dados
def input_data():
    print("Por favor, insira seu código linha por linha.")
    print("Digite '.' em uma linha separada para finalizar a entrada.")
    lines = ""
    line_no = 1
    line = input(f"{line_no:2d} > ")
    while line != ".":
        lines = lines + "\n" + line
        line_no = line_no + 1
        line = input(f"{line_no:2d} > ")
    return lines

# Função principal
def main():
    data = input_data()
    result = test_parser(data)
    print(result)
    
    # print(test_lexer(data))

if __name__ == "__main__":
    main()
