
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND BOOL BREAK CHAR DIVIDE DO DOT ELSE EQ EQUALS FALSE GE GT HASH ID IF INT LBRACE LBRACK LE LPAREN LT MINUS NEG NEQ NOT NUMBER_FLOAT NUMBER_INT OR PLUS PRINT RBRACE RBRACK READ RECORD RPAREN SEMICOLON TIMES TRUE WHILEprogram : blockblock : LBRACE declaration statements RBRACEdeclaration : type ID SEMICOLON declaration\n                   | empty type : basic dimensions\n             | basic HASH\n             | basic\n             | RECORD LBRACE declaration RBRACEdimensions : LBRACK NUMBER_INT RBRACK dimensions\n                  | LBRACK NUMBER_INT RBRACKbasic : INT\n             | CHAR\n             | BOOLstatements : statement statements\n                  | emptystatement : local EQUALS boolean SEMICOLON\n                 | conditional\n                 | while_loop\n                 | do_while_loop\n                 | BREAK SEMICOLON\n                 | print\n                 | read\n                 | blockprint : PRINT LPAREN boolean RPAREN SEMICOLONread : READ LPAREN local RPAREN SEMICOLONconditional : IF LPAREN boolean RPAREN statement conditional_auxconditional_aux : ELSE statement\n                       | emptywhile_loop : WHILE LPAREN boolean RPAREN statementdo_while_loop : DO statement WHILE LPAREN boolean RPAREN SEMICOLONlocal : ID\n             | HASH ID\n             | ID local_aux\n             | HASH ID local_auxlocal_aux : offset\n                 | fieldoffset : LBRACK boolean RBRACK offset\n              | emptyfield : DOT factor field\n             | DOT factorboolean : join\n               | boolean OR joinjoin : equality\n            | join AND equality equality : relational \n                | equality EQ relational\n                | equality NEQ relationalrelational : expression \n                  | relational_operator expressionrelational_operator : LT\n                           | LE\n                           | GT\n                           | GEexpression : term \n                | expression expression_operator termexpression_operator : PLUS\n                           | MINUSterm : unary\n            | term TIMES unary\n            | term DIVIDE unaryunary : NOT unary\n             | NEG unary\n             | factorfactor : LPAREN boolean RPAREN\n              | local\n              | NUMBER_INT\n              | NUMBER_FLOAT\n              | FALSE\n              | TRUEempty :'
    
_lr_action_items = {'LBRACE':([0,3,4,6,8,13,16,17,18,20,21,22,27,35,38,51,83,86,102,103,117,118,120,121,122,123,124,126,127,],[3,-70,3,-4,34,3,-17,-18,-19,-21,-22,-23,3,-2,-20,-70,-3,-16,3,3,-70,-29,-24,-25,-26,3,-28,-27,-30,]),'$end':([1,2,35,],[0,-1,-2,]),'RECORD':([3,34,51,],[8,8,8,]),'BREAK':([3,4,6,13,16,17,18,20,21,22,27,35,38,51,83,86,102,103,117,118,120,121,122,123,124,126,127,],[-70,19,-4,19,-17,-18,-19,-21,-22,-23,19,-2,-20,-70,-3,-16,19,19,-70,-29,-24,-25,-26,19,-28,-27,-30,]),'ID':([3,4,5,6,7,9,10,11,13,16,17,18,20,21,22,24,27,31,32,35,37,38,42,44,46,47,49,50,51,60,62,63,64,65,67,68,70,83,84,85,86,87,88,89,90,91,92,93,95,96,102,103,104,107,117,118,120,121,122,123,124,126,127,],[-70,23,30,-4,-7,-11,-12,-13,23,-17,-18,-19,-21,-22,-23,45,23,-5,-6,-2,23,-20,23,23,23,23,23,23,-70,23,-50,-51,-52,-53,23,23,23,-3,-10,-8,-16,23,23,23,23,23,-56,-57,23,23,23,23,23,-9,-70,-29,-24,-25,-26,23,-28,-27,-30,]),'HASH':([3,4,6,7,9,10,11,13,16,17,18,20,21,22,27,35,37,38,42,44,46,47,49,50,51,60,62,63,64,65,67,68,70,83,86,87,88,89,90,91,92,93,95,96,102,103,104,117,118,120,121,122,123,124,126,127,],[-70,24,-4,32,-11,-12,-13,24,-17,-18,-19,-21,-22,-23,24,-2,24,-20,24,24,24,24,24,24,-70,24,-50,-51,-52,-53,24,24,24,-3,-16,24,24,24,24,24,-56,-57,24,24,24,24,24,-70,-29,-24,-25,-26,24,-28,-27,-30,]),'IF':([3,4,6,13,16,17,18,20,21,22,27,35,38,51,83,86,102,103,117,118,120,121,122,123,124,126,127,],[-70,25,-4,25,-17,-18,-19,-21,-22,-23,25,-2,-20,-70,-3,-16,25,25,-70,-29,-24,-25,-26,25,-28,-27,-30,]),'WHILE':([3,4,6,13,16,17,18,20,21,22,27,35,38,48,51,83,86,102,103,117,118,120,121,122,123,124,126,127,],[-70,26,-4,26,-17,-18,-19,-21,-22,-23,26,-2,-20,80,-70,-3,-16,26,26,-70,-29,-24,-25,-26,26,-28,-27,-30,]),'DO':([3,4,6,13,16,17,18,20,21,22,27,35,38,51,83,86,102,103,117,118,120,121,122,123,124,126,127,],[-70,27,-4,27,-17,-18,-19,-21,-22,-23,27,-2,-20,-70,-3,-16,27,27,-70,-29,-24,-25,-26,27,-28,-27,-30,]),'PRINT':([3,4,6,13,16,17,18,20,21,22,27,35,38,51,83,86,102,103,117,118,120,121,122,123,124,126,127,],[-70,28,-4,28,-17,-18,-19,-21,-22,-23,28,-2,-20,-70,-3,-16,28,28,-70,-29,-24,-25,-26,28,-28,-27,-30,]),'READ':([3,4,6,13,16,17,18,20,21,22,27,35,38,51,83,86,102,103,117,118,120,121,122,123,124,126,127,],[-70,29,-4,29,-17,-18,-19,-21,-22,-23,29,-2,-20,-70,-3,-16,29,29,-70,-29,-24,-25,-26,29,-28,-27,-30,]),'RBRACE':([3,4,6,12,13,14,16,17,18,20,21,22,34,35,36,38,51,53,83,86,117,118,120,121,122,124,126,127,],[-70,-70,-4,35,-70,-15,-17,-18,-19,-21,-22,-23,-70,-2,-14,-20,-70,85,-3,-16,-70,-29,-24,-25,-26,-28,-27,-30,]),'INT':([3,34,51,],[9,9,9,]),'CHAR':([3,34,51,],[10,10,10,]),'BOOL':([3,34,51,],[11,11,11,]),'LBRACK':([7,9,10,11,23,45,84,100,],[33,-11,-12,-13,42,42,33,42,]),'EQUALS':([15,23,39,40,41,43,45,54,71,72,73,74,76,77,100,101,115,116,],[37,-31,-33,-35,-36,-38,-32,-65,-66,-67,-68,-69,-40,-34,-70,-39,-64,-37,]),'ELSE':([16,17,18,20,21,22,35,38,86,117,118,120,121,122,124,126,127,],[-17,-18,-19,-21,-22,-23,-2,-20,-16,123,-29,-24,-25,-26,-28,-27,-30,]),'SEMICOLON':([19,23,30,39,40,41,43,45,54,55,56,57,58,59,61,66,69,71,72,73,74,76,77,94,97,98,100,101,105,106,108,109,110,111,112,113,114,115,116,125,],[38,-31,51,-33,-35,-36,-38,-32,-65,86,-41,-43,-45,-48,-54,-58,-63,-66,-67,-68,-69,-40,-34,-49,-61,-62,-70,-39,120,121,-42,-44,-46,-47,-55,-59,-60,-64,-37,127,]),'TIMES':([23,39,40,41,43,45,54,61,66,69,71,72,73,74,76,77,97,98,100,101,112,113,114,115,116,],[-31,-33,-35,-36,-38,-32,-65,95,-58,-63,-66,-67,-68,-69,-40,-34,-61,-62,-70,-39,95,-59,-60,-64,-37,]),'DIVIDE':([23,39,40,41,43,45,54,61,66,69,71,72,73,74,76,77,97,98,100,101,112,113,114,115,116,],[-31,-33,-35,-36,-38,-32,-65,96,-58,-63,-66,-67,-68,-69,-40,-34,-61,-62,-70,-39,96,-59,-60,-64,-37,]),'PLUS':([23,39,40,41,43,45,54,59,61,66,69,71,72,73,74,76,77,94,97,98,100,101,112,113,114,115,116,],[-31,-33,-35,-36,-38,-32,-65,92,-54,-58,-63,-66,-67,-68,-69,-40,-34,92,-61,-62,-70,-39,-55,-59,-60,-64,-37,]),'MINUS':([23,39,40,41,43,45,54,59,61,66,69,71,72,73,74,76,77,94,97,98,100,101,112,113,114,115,116,],[-31,-33,-35,-36,-38,-32,-65,93,-54,-58,-63,-66,-67,-68,-69,-40,-34,93,-61,-62,-70,-39,-55,-59,-60,-64,-37,]),'EQ':([23,39,40,41,43,45,54,57,58,59,61,66,69,71,72,73,74,76,77,94,97,98,100,101,109,110,111,112,113,114,115,116,],[-31,-33,-35,-36,-38,-32,-65,89,-45,-48,-54,-58,-63,-66,-67,-68,-69,-40,-34,-49,-61,-62,-70,-39,89,-46,-47,-55,-59,-60,-64,-37,]),'NEQ':([23,39,40,41,43,45,54,57,58,59,61,66,69,71,72,73,74,76,77,94,97,98,100,101,109,110,111,112,113,114,115,116,],[-31,-33,-35,-36,-38,-32,-65,90,-45,-48,-54,-58,-63,-66,-67,-68,-69,-40,-34,-49,-61,-62,-70,-39,90,-46,-47,-55,-59,-60,-64,-37,]),'AND':([23,39,40,41,43,45,54,56,57,58,59,61,66,69,71,72,73,74,76,77,94,97,98,100,101,108,109,110,111,112,113,114,115,116,],[-31,-33,-35,-36,-38,-32,-65,88,-43,-45,-48,-54,-58,-63,-66,-67,-68,-69,-40,-34,-49,-61,-62,-70,-39,88,-44,-46,-47,-55,-59,-60,-64,-37,]),'OR':([23,39,40,41,43,45,54,55,56,57,58,59,61,66,69,71,72,73,74,75,76,77,78,79,81,94,97,98,99,100,101,108,109,110,111,112,113,114,115,116,119,],[-31,-33,-35,-36,-38,-32,-65,87,-41,-43,-45,-48,-54,-58,-63,-66,-67,-68,-69,87,-40,-34,87,87,87,-49,-61,-62,87,-70,-39,-42,-44,-46,-47,-55,-59,-60,-64,-37,87,]),'RBRACK':([23,39,40,41,43,45,52,54,56,57,58,59,61,66,69,71,72,73,74,75,76,77,94,97,98,100,101,108,109,110,111,112,113,114,115,116,],[-31,-33,-35,-36,-38,-32,84,-65,-41,-43,-45,-48,-54,-58,-63,-66,-67,-68,-69,100,-40,-34,-49,-61,-62,-70,-39,-42,-44,-46,-47,-55,-59,-60,-64,-37,]),'DOT':([23,39,40,41,43,45,54,71,72,73,74,76,77,100,101,115,116,],[44,-33,-35,-36,-38,44,-65,-66,-67,-68,-69,44,-34,-70,-39,-64,-37,]),'RPAREN':([23,39,40,41,43,45,54,56,57,58,59,61,66,69,71,72,73,74,76,77,78,79,81,82,94,97,98,99,100,101,108,109,110,111,112,113,114,115,116,119,],[-31,-33,-35,-36,-38,-32,-65,-41,-43,-45,-48,-54,-58,-63,-66,-67,-68,-69,-40,-34,102,103,105,106,-49,-61,-62,115,-70,-39,-42,-44,-46,-47,-55,-59,-60,-64,-37,125,]),'LPAREN':([25,26,28,29,37,42,44,46,47,49,60,62,63,64,65,67,68,70,80,87,88,89,90,91,92,93,95,96,104,],[46,47,49,50,70,70,70,70,70,70,70,-50,-51,-52,-53,70,70,70,104,70,70,70,70,70,-56,-57,70,70,70,]),'NUMBER_INT':([33,37,42,44,46,47,49,60,62,63,64,65,67,68,70,87,88,89,90,91,92,93,95,96,104,],[52,71,71,71,71,71,71,71,-50,-51,-52,-53,71,71,71,71,71,71,71,71,-56,-57,71,71,71,]),'LT':([37,42,46,47,49,70,87,88,89,90,104,],[62,62,62,62,62,62,62,62,62,62,62,]),'LE':([37,42,46,47,49,70,87,88,89,90,104,],[63,63,63,63,63,63,63,63,63,63,63,]),'GT':([37,42,46,47,49,70,87,88,89,90,104,],[64,64,64,64,64,64,64,64,64,64,64,]),'GE':([37,42,46,47,49,70,87,88,89,90,104,],[65,65,65,65,65,65,65,65,65,65,65,]),'NOT':([37,42,46,47,49,60,62,63,64,65,67,68,70,87,88,89,90,91,92,93,95,96,104,],[67,67,67,67,67,67,-50,-51,-52,-53,67,67,67,67,67,67,67,67,-56,-57,67,67,67,]),'NEG':([37,42,46,47,49,60,62,63,64,65,67,68,70,87,88,89,90,91,92,93,95,96,104,],[68,68,68,68,68,68,-50,-51,-52,-53,68,68,68,68,68,68,68,68,-56,-57,68,68,68,]),'NUMBER_FLOAT':([37,42,44,46,47,49,60,62,63,64,65,67,68,70,87,88,89,90,91,92,93,95,96,104,],[72,72,72,72,72,72,72,-50,-51,-52,-53,72,72,72,72,72,72,72,72,-56,-57,72,72,72,]),'FALSE':([37,42,44,46,47,49,60,62,63,64,65,67,68,70,87,88,89,90,91,92,93,95,96,104,],[73,73,73,73,73,73,73,-50,-51,-52,-53,73,73,73,73,73,73,73,73,-56,-57,73,73,73,]),'TRUE':([37,42,44,46,47,49,60,62,63,64,65,67,68,70,87,88,89,90,91,92,93,95,96,104,],[74,74,74,74,74,74,74,-50,-51,-52,-53,74,74,74,74,74,74,74,74,-56,-57,74,74,74,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'block':([0,4,13,27,102,103,123,],[2,22,22,22,22,22,22,]),'declaration':([3,34,51,],[4,53,83,]),'type':([3,34,51,],[5,5,5,]),'empty':([3,4,13,23,34,45,51,100,117,],[6,14,14,43,6,43,6,43,124,]),'basic':([3,34,51,],[7,7,7,]),'statements':([4,13,],[12,36,]),'statement':([4,13,27,102,103,123,],[13,13,48,117,118,126,]),'local':([4,13,27,37,42,44,46,47,49,50,60,67,68,70,87,88,89,90,91,95,96,102,103,104,123,],[15,15,15,54,54,54,54,54,54,82,54,54,54,54,54,54,54,54,54,54,54,15,15,54,15,]),'conditional':([4,13,27,102,103,123,],[16,16,16,16,16,16,]),'while_loop':([4,13,27,102,103,123,],[17,17,17,17,17,17,]),'do_while_loop':([4,13,27,102,103,123,],[18,18,18,18,18,18,]),'print':([4,13,27,102,103,123,],[20,20,20,20,20,20,]),'read':([4,13,27,102,103,123,],[21,21,21,21,21,21,]),'dimensions':([7,84,],[31,107,]),'local_aux':([23,45,],[39,77,]),'offset':([23,45,100,],[40,40,116,]),'field':([23,45,76,],[41,41,101,]),'boolean':([37,42,46,47,49,70,104,],[55,75,78,79,81,99,119,]),'join':([37,42,46,47,49,70,87,104,],[56,56,56,56,56,56,108,56,]),'equality':([37,42,46,47,49,70,87,88,104,],[57,57,57,57,57,57,57,109,57,]),'relational':([37,42,46,47,49,70,87,88,89,90,104,],[58,58,58,58,58,58,58,58,110,111,58,]),'expression':([37,42,46,47,49,60,70,87,88,89,90,104,],[59,59,59,59,59,94,59,59,59,59,59,59,]),'relational_operator':([37,42,46,47,49,70,87,88,89,90,104,],[60,60,60,60,60,60,60,60,60,60,60,]),'term':([37,42,46,47,49,60,70,87,88,89,90,91,104,],[61,61,61,61,61,61,61,61,61,61,61,112,61,]),'unary':([37,42,46,47,49,60,67,68,70,87,88,89,90,91,95,96,104,],[66,66,66,66,66,66,97,98,66,66,66,66,66,66,113,114,66,]),'factor':([37,42,44,46,47,49,60,67,68,70,87,88,89,90,91,95,96,104,],[69,69,76,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,]),'expression_operator':([59,94,],[91,91,]),'conditional_aux':([117,],[122,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> block','program',1,'p_program','principal.py',131),
  ('block -> LBRACE declaration statements RBRACE','block',4,'p_block','principal.py',135),
  ('declaration -> type ID SEMICOLON declaration','declaration',4,'p_declaration','principal.py',140),
  ('declaration -> empty','declaration',1,'p_declaration','principal.py',141),
  ('type -> basic dimensions','type',2,'p_type','principal.py',148),
  ('type -> basic HASH','type',2,'p_type','principal.py',149),
  ('type -> basic','type',1,'p_type','principal.py',150),
  ('type -> RECORD LBRACE declaration RBRACE','type',4,'p_type','principal.py',151),
  ('dimensions -> LBRACK NUMBER_INT RBRACK dimensions','dimensions',4,'p_dimensions','principal.py',160),
  ('dimensions -> LBRACK NUMBER_INT RBRACK','dimensions',3,'p_dimensions','principal.py',161),
  ('basic -> INT','basic',1,'p_basic','principal.py',168),
  ('basic -> CHAR','basic',1,'p_basic','principal.py',169),
  ('basic -> BOOL','basic',1,'p_basic','principal.py',170),
  ('statements -> statement statements','statements',2,'p_statements','principal.py',174),
  ('statements -> empty','statements',1,'p_statements','principal.py',175),
  ('statement -> local EQUALS boolean SEMICOLON','statement',4,'p_statement','principal.py',182),
  ('statement -> conditional','statement',1,'p_statement','principal.py',183),
  ('statement -> while_loop','statement',1,'p_statement','principal.py',184),
  ('statement -> do_while_loop','statement',1,'p_statement','principal.py',185),
  ('statement -> BREAK SEMICOLON','statement',2,'p_statement','principal.py',186),
  ('statement -> print','statement',1,'p_statement','principal.py',187),
  ('statement -> read','statement',1,'p_statement','principal.py',188),
  ('statement -> block','statement',1,'p_statement','principal.py',189),
  ('print -> PRINT LPAREN boolean RPAREN SEMICOLON','print',5,'p_print','principal.py',200),
  ('read -> READ LPAREN local RPAREN SEMICOLON','read',5,'p_read','principal.py',204),
  ('conditional -> IF LPAREN boolean RPAREN statement conditional_aux','conditional',6,'p_conditional','principal.py',208),
  ('conditional_aux -> ELSE statement','conditional_aux',2,'p_conditional_aux','principal.py',212),
  ('conditional_aux -> empty','conditional_aux',1,'p_conditional_aux','principal.py',213),
  ('while_loop -> WHILE LPAREN boolean RPAREN statement','while_loop',5,'p_while_loop','principal.py',220),
  ('do_while_loop -> DO statement WHILE LPAREN boolean RPAREN SEMICOLON','do_while_loop',7,'p_do_while_loop','principal.py',224),
  ('local -> ID','local',1,'p_local','principal.py',228),
  ('local -> HASH ID','local',2,'p_local','principal.py',229),
  ('local -> ID local_aux','local',2,'p_local','principal.py',230),
  ('local -> HASH ID local_aux','local',3,'p_local','principal.py',231),
  ('local_aux -> offset','local_aux',1,'p_local_aux','principal.py',240),
  ('local_aux -> field','local_aux',1,'p_local_aux','principal.py',241),
  ('offset -> LBRACK boolean RBRACK offset','offset',4,'p_offset','principal.py',245),
  ('offset -> empty','offset',1,'p_offset','principal.py',246),
  ('field -> DOT factor field','field',3,'p_field','principal.py',253),
  ('field -> DOT factor','field',2,'p_field','principal.py',254),
  ('boolean -> join','boolean',1,'p_boolean','principal.py',261),
  ('boolean -> boolean OR join','boolean',3,'p_boolean','principal.py',262),
  ('join -> equality','join',1,'p_join','principal.py',269),
  ('join -> join AND equality','join',3,'p_join','principal.py',270),
  ('equality -> relational','equality',1,'p_equality','principal.py',277),
  ('equality -> equality EQ relational','equality',3,'p_equality','principal.py',278),
  ('equality -> equality NEQ relational','equality',3,'p_equality','principal.py',279),
  ('relational -> expression','relational',1,'p_relational','principal.py',286),
  ('relational -> relational_operator expression','relational',2,'p_relational','principal.py',287),
  ('relational_operator -> LT','relational_operator',1,'p_relational_operator','principal.py',294),
  ('relational_operator -> LE','relational_operator',1,'p_relational_operator','principal.py',295),
  ('relational_operator -> GT','relational_operator',1,'p_relational_operator','principal.py',296),
  ('relational_operator -> GE','relational_operator',1,'p_relational_operator','principal.py',297),
  ('expression -> term','expression',1,'p_expression','principal.py',301),
  ('expression -> expression expression_operator term','expression',3,'p_expression','principal.py',302),
  ('expression_operator -> PLUS','expression_operator',1,'p_expression_operator','principal.py',309),
  ('expression_operator -> MINUS','expression_operator',1,'p_expression_operator','principal.py',310),
  ('term -> unary','term',1,'p_term','principal.py',314),
  ('term -> term TIMES unary','term',3,'p_term','principal.py',315),
  ('term -> term DIVIDE unary','term',3,'p_term','principal.py',316),
  ('unary -> NOT unary','unary',2,'p_unary','principal.py',323),
  ('unary -> NEG unary','unary',2,'p_unary','principal.py',324),
  ('unary -> factor','unary',1,'p_unary','principal.py',325),
  ('factor -> LPAREN boolean RPAREN','factor',3,'p_factor','principal.py',332),
  ('factor -> local','factor',1,'p_factor','principal.py',333),
  ('factor -> NUMBER_INT','factor',1,'p_factor','principal.py',334),
  ('factor -> NUMBER_FLOAT','factor',1,'p_factor','principal.py',335),
  ('factor -> FALSE','factor',1,'p_factor','principal.py',336),
  ('factor -> TRUE','factor',1,'p_factor','principal.py',337),
  ('empty -> <empty>','empty',0,'p_empty','principal.py',344),
]
