import ply.lex as lex
import ply.yacc as yacc
import math as math

no_error = 0

# Definición de la tabla de símbolos
class SymbolTable:
    def __init__(self):
        self.symbols = {}

    def add_symbol(self, name, value):
        self.symbols[name] = value

    def get_symbol(self, name):
        return self.symbols.get(name, None)

# Inicialización de la tabla de símbolos
symbol_table = SymbolTable()

# Lista de nombres de tokens
tokens = (
    'PRINT',
    'NUMBER',
    'OPER',
    'STR',
    'VAR',
    'LDEL',
    'RDEL',
    'EQUAL',
    'TYPE',
    'CONDICION',
    #'CICLO'
)

# Expresiones regulares para tokens
t_NUMBER = r'\d+(\.\d+)?'
t_OPER = r'[-+*/]'
t_STR = r'("[^"]*"|\'[^\']*\')'
t_LDEL = r'[({\[]'
t_RDEL = r'[)}\]]'
t_EQUAL = r'(=|\+=|-=)'

# Función para tratar palabras reservadas
def t_PRINT(t):
    r'\bprint\b'
    return t

def t_CONDICION(t):
    r'(if|do)'
    return t

# def t_CICLO(t):
#     r'(iniciclo|finciclo)'
#     return t

# Función para manejar los tipos de dato
def t_TYPE(t):
    r'(INT|int|FLO|flo|BOL|bol)'
    return t

# Función para tratar nombres de variables
def t_VAR(t):
     r'[a-zA-Z]\w*'
     return t

# Ignorar espacios y tabulaciones
t_ignore = ' \t'

def t_error(t):
    global no_error
    print("Error léxico: Carácter inesperado: '%s'" % t.value[0])
    no_error += 1
    t.lexer.skip(1)
# Crear el analizador léxico
lexer = lex.lex()

#PARSER
precedence = (
    ('left', 'OPER'),
)

# Define la gramática
def p_program(p):
    '''
    program : assignment
            | print_statement
            | conditional_statement
            | verf
            | result
            | expression
    '''

def p_assignment(p):
    '''
    assignment : TYPE VAR EQUAL NUMBER
    '''
    if p[1] == 'int':
        num=float(p[4])
        num=int(num)
        symbol_table.add_symbol(p[2],num)
    elif p[1] == 'flo':
        symbol_table.add_symbol(p[2],p[4])
    elif p[1] == 'bol':
        num=int(p[4])
        if num == 0 or num == 1:
            symbol_table.add_symbol(p[2],p[4])
        else:
            print("Error. Not a boolean")

def p_expression_arithmetic(p):
    '''
    expression : NUMBER
               | expression OPER expression
    '''
    if len(p) == 2:
        # Caso base: solo un número
        p[0] = float(p[1])
    else:
        # Caso recursivo: expresión con operador
        num1 = p[1]
        operator = p[2]
        num2 = p[3]

        if operator == '+':
            p[0] = num1 + num2
        elif operator == '-':
            p[0] = num1 - num2
        elif operator == '*':
            p[0] = num1 * num2
        elif operator == '/':
            if num2 != 0:
                p[0] = num1 / num2
            else:
                print("Error: División por cero.")
                p[0] = 0  # Podrías elegir manejar el error de otra manera
    print(p[0])

def p_print_statement(p):
  '''
  print_statement : PRINT (VAR | LDEL STR RDEL)
  '''
  if isinstance(p[2], str):  # Check if it's a string
    print(p[2])  # Print the string directly
  else:  # It's a variable
    variable_name = p[2]
    variable_value = symbol_table.get_symbol(variable_name)
    if variable_value is not None:
      print(variable_value)  # Print the variable value
    else:
      print(f"Error: Variable '{variable_name}' no definida.")

    
def p_conditional_statement(p):
    '''
    conditional_statement : CONDICION VAR EQUAL NUMBER CONDICION TYPE VAR EQUAL NUMBER
    '''
    variable_name = p[2]
    comparison_number = float(p[4])
    current_variable_value = symbol_table.get_symbol(variable_name)

    if current_variable_value is not None:
        current_variable_value = float(current_variable_value)
        if current_variable_value == comparison_number:
            symbol_table.add_symbol(p[7],float(p[9]))
        else:
            p[0] = None  # La condición no se cumple, no se ejecuta el bloque condicional
    else:
        print(f"Error: Variable '{variable_name}' no definida.")
        p[0] = None  # Variable no definida, no se ejecuta el bloque condicional
    
def p_verf(p):
    '''
    verf : VAR EQUAL NUMBER
    '''
    variable_value = symbol_table.get_symbol(p[1])
    if variable_value is not None:
        variable_value = float(variable_value)
        if variable_value == float(p[3]):
            print("True")
        else:
            print("False")
    else:
        print(f"Error: Variable '{p[1]}' no definida.")

def p_result(p):
    '''
    result : VAR
    '''
    print(symbol_table.get_symbol(p[1]))

def p_print_statement(p):
    '''
    print_statement : PRINT LDEL STR RDEL
    '''
    print(p[3])

def p_print_var(p):
    '''
    print_var : PRINT VAR
    '''
    variable_name = p[2]
    variable_value = symbol_table.get_symbol(variable_name)
    if variable_value is not None:
        print(variable_value)
    else:
        print(f"Error: Variable '{variable_name}' no definida.")



# Definir la regla para manejar errores sintácticos

def p_error(p):
    global no_error
    if p:
        print(f"Error sintáctico en posición {p.lexpos}: Token inesperado '{p.value}'")
    else:
        print("Error sintáctico: Fin de entrada inesperado")

# Construir el parser
parser = yacc.yacc(start='program')

while True:
    try:
        s = input('Interprete > ')
    except EOFError:
        break
    if not s:
        continue
    if s == "exit":
        break
    yacc.parse(s)
