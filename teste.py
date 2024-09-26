import ply.yacc as yacc
import ply.lex as lex

# Definição dos tokens
tokens = [
    'ID', 'NUMBER', 'TRUE', 'FALSE', 'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'EQ', 'NEQ', 'LT', 'LE', 'GT', 'GE', 'EQUALS', 'LPAREN', 'RPAREN',
    'LBRACE', 'RBRACE', 'SEMICOLON', 'COMMA', 'DOT', 'BREAK', 'PRINT', 'READ',
    'IF', 'ELSE', 'WHILE', 'DO', 'INT', 'CHAR', 'BOOL', 'NOT', 'RECORD'
]

# Definindo os padrões para os tokens
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
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','
t_DOT = r'\.'
t_BREAK = r'break'
t_PRINT = r'print'
t_READ = r'read'
t_TRUE = r'true'
t_FALSE = r'false'
t_NOT = r'!'

# Palavras-chave (reservadas)
reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'break': 'BREAK',
    'print': 'PRINT',
    'read': 'READ',
    'int': 'INT',
    'char': 'CHAR',
    'bool': 'BOOL',
    'record': 'RECORD'
}

tokens += list(reserved.values())

# Ignorar espaços em branco
t_ignore = ' \t'

# Definindo identificadores e números
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'ID')  # Define como palavra reservada ou ID
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

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

# Definição da gramática (Analisador Sintático)
def p_program(p):
    '''program : block'''
    p[0] = p[1]

def p_block(p):
    '''block : LBRACE decls stmts RBRACE'''
    p[0] = ('block', p[2], p[3])

def p_decls(p):
    '''decls : decls decl
             | empty'''
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = []

def p_decl(p):
    '''decl : type ID SEMICOLON'''
    p[0] = ('decl', p[1], p[2])

def p_type(p):
    '''type : INT dims
            | CHAR dims
            | BOOL dims
            | record'''
    p[0] = p[1:]

def p_dims(p):
    '''dims : LBRACE NUMBER RBRACE dims
            | empty'''
    p[0] = p[1:] if len(p) > 1 else []

def p_record(p):
    '''record : RECORD LBRACE decls RBRACE'''
    p[0] = ('record', p[3])

def p_stmts(p):
    '''stmts : stmt stmts
             | empty'''
    if len(p) == 3:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []

def p_stmt(p):
    '''stmt : loc EQUALS bool SEMICOLON
            | IF LPAREN bool RPAREN stmt
            | IF LPAREN bool RPAREN stmt ELSE stmt
            | WHILE LPAREN bool RPAREN stmt
            | DO stmt WHILE LPAREN bool RPAREN SEMICOLON
            | BREAK SEMICOLON
            | PRINT LPAREN bool RPAREN SEMICOLON
            | READ LPAREN loc RPAREN SEMICOLON
            | block'''
    p[0] = ('stmt', p[1:])

def p_loc(p):
    '''loc : ID
           | ID DOT ID
           | empty'''
    p[0] = p[1:]

def p_bool(p):
    '''bool : join
            | empty'''
    p[0] = p[1]

def p_join(p):
    '''join : equality
            | join PLUS equality
            | join MINUS equality'''
    p[0] = p[1:]

def p_equality(p):
    '''equality : rel
                | equality EQ rel
                | equality NEQ rel'''
    p[0] = p[1:]

def p_rel(p):
    '''rel : expr
           | expr LT expr
           | expr LE expr
           | expr GT expr
           | expr GE expr'''
    p[0] = p[1:]

def p_expr(p):
    '''expr : term
            | expr PLUS term
            | expr MINUS term'''
    p[0] = p[1:]

def p_term(p):
    '''term : unary
            | term TIMES unary
            | term DIVIDE unary'''
    p[0] = p[1:]

def p_unary(p):
    '''unary : factor
             | MINUS unary
             | NOT unary'''
    p[0] = p[1:]

def p_factor(p):
    '''factor : LPAREN bool RPAREN
              | loc
              | NUMBER
              | TRUE
              | FALSE'''
    p[0] = p[1:]

def p_empty(p):
    '''empty :'''
    p[0] = None

# Tratamento de erros de sintaxe
def p_error(p):
    print(f"Erro de sintaxe em '{p.value}'" if p else "Erro de sintaxe no final da entrada")

# Construir o parser
parser = yacc.yacc()

# Função de teste para o lexer
def test_lexer(data):
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)

# Função de teste para o parser
def test_parser(data):
    result = parser.parse(data)
    return result

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

if __name__ == "__main__":
    main()
