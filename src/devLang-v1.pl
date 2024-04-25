% DCG parse tree:
:- table conditional_logic/3 ,variable/3.
:- use_rendering(svgtree).

% Program 
procedure(proc(X)) --> ['dev'], block(X), ['lang'].
block(blk(X)) --> ['{'], statement_pipeline(X), ['}'].
statement_pipeline(stmt_pipe(X,Z)) --> statement(X), comma, statement_pipeline(Z).  
statement_pipeline(stmt_pipe(X)) --> statement(X).


% Data Types
data_type(data_type_structure(X)) --> bool(X). 
data_type(data_type_structure(X)) --> int(X). 
data_type(data_type_structure(X)) --> charr(X). 

    % Boolean Data type :  TODO Change this  - check if node name has quotes or not. check tabling. 
    bool(bool_structure('true')) --> ['bool(true)']. 
    bool(bool_structure('false')) --> ['bool(false)'].
    bool(bool_structure(X)) --> conditional_logic(X).
        % Conditional Expressions.
        conditional_logic(cond_log(X)) --> logical_comparison(X).
        conditional_logic(cond_log(X)) --> integer_comparison(X). 
            % And, Or, Not Gates
            logical_comparison(log_comp(X,Z)) --> ['and'], ['('], bool(X), comma, bool(Z), [')'].
            logical_comparison(log_comp(X,Z)) --> ['or'], ['('], bool(X), comma, bool(Z), [')'].
            logical_comparison(log_comp(X)) --> ['not'], ['('], bool(X) ,[')'].
            % integer comparison
            integer_comparison(int_comp(X,Y,Z)) --> int(X), comparison_operator(Y), int(Z).
                % Comparison Operator
                comparison_operator(comp_op(>)) --> ['>']. 
                comparison_operator(comp_op(<)) --> ['<'].
                comparison_operator(comp_op(=)) --> ['=='].

    
    % Integer Defination  % Loop ( remove loop ) TODO Add Tabling
    int(int_structure(X)) --> ['int('], numbers(X) , [')']. 
    int(int_structure(X)) --> expression(X).
        % arithematic Expression ( interger functions )
        expression_part(expr_part(X)) --> int(X). 
        expression_part(expr_part(X)) --> variable(X).
        expression(expr(X,Y,Z)) --> ['['], expression_part(X), operator(Y), expression_part(Z), [']'].
        % Operator
            operator(op(+)) --> ['+']. 
            operator(op(-)) --> ['-']. 
            operator(op()) --> ['']. 
            operator(op(/)) --> ['/'].

    % String ( character Array )
    charr(char(X)) --> ['charr('], character(X), [')'].
         % concatination TODO
         % Access nth Char TODO 

% Statement types
statement(stmt(X)) --> null_statements(X).
statement(stmt(X)) --> print_statements(X).
statement(stmt(X)) --> assignment_statement(X).
statement(stmt(X)) --> conditional_statement(X).
statement(stmt(X)) --> loops(X).

    %   Null Statements
    null_statements(' ') --> [' '].

    %  Print Statements
    print_statements(print_stmt(X)) --> ['tout'], ['('], data_type(X), [')'].
    print_statements(print_stmt(X)) --> ['tout'], ['('], variable(X), [')'].

    % assignment Statements TODO
    assignment_statement(assign_stmt(X,Z)) --> variable(X),['='], data_type(Z). 
        % Variable TODO define how to tokenize Variable
        % variable(var(X)) --> ['let'], var(X).
        variable(identifier(X)) --> ['var-[a-z]'].

    % Conditional Statements
    conditional_statement(cond_stmt(X,Y,Z)) --> ['if'], ['('], bool(X), [')'], block(Y), ['otherwise'], block(Z).
    conditional_statement(cond_stmt(X,Y,Z)) --> ['?'], bool(X), [':'], statement_pipeline(Y), [':'], statement_pipeline(Z).

    % Loops
    loops(loops(X,Y)) --> loop_part(X), block(Y). 
    loops(loops(X,Y)) --> loopwith_part(X), block(Y). 
    loops(loops(X,Y)) --> looprange_part(X), block(Y).
        % while loop
        loop_part(loop_part(X)) --> ['loop'], ['('], conditional_logic(X), [')'].
        % for loop
        loopwith_part(loop_with(X,Y)) --> ['loopwith'], ['('], assignment_statement(X), [':'], conditional_logic(Y), [')'].
        % range loop
        looprange_part(loop_range(X,Y,Z)) --> ['looprange'], ['('], assignment_statement(X),  int(Z), [')'].

% Characters allowed
character(character(X))--> lowercase_char(X). 
character(character(X))--> uppercase_char(X).
character(character(X))--> numbers(X).
    lowercase_char(lc(a)) --> ['a'].
    lowercase_char(lc(b)) --> ['b'].
    lowercase_char(lc(c)) --> ['c'].
    lowercase_char(lc(d)) --> ['d'].
    lowercase_char(lc(e)) --> ['e'].
    lowercase_char(lc(f)) --> ['f'].
    lowercase_char(lc(g)) --> ['g'].
    lowercase_char(lc(h)) --> ['h'].
    lowercase_char(lc(i)) --> ['i'].
    lowercase_char(lc(j)) --> ['j'].
    lowercase_char(lc(k)) --> ['k'].

    uppercase_char(uc(A)) --> ['A'].
    uppercase_char(uc(B)) --> ['B'].
    uppercase_char(uc(C)) --> ['C'].
    uppercase_char(uc(D)) --> ['D'].
    uppercase_char(uc(E)) --> ['E'].
    uppercase_char(uc(F)) --> ['F'].
    uppercase_char(uc(G)) --> ['G'].
    uppercase_char(uc(H)) --> ['H'].
    uppercase_char(uc(I)) --> ['I'].
    uppercase_char(uc(J)) --> ['J'].
    uppercase_char(uc(K)) --> ['K'].

    numbers(num(0)) --> ['0'].
    numbers(num(1)) --> ['1'].
    numbers(num(2)) --> ['2'].
    numbers(num(3)) --> ['3'].
    numbers(num(4)) --> ['4'].
    numbers(num(5)) --> ['5'].
    numbers(num(6)) --> ['6'].
    numbers(num(7)) --> ['7'].
    numbers(num(8)) --> ['8'].
    numbers(num(9)) --> ['9'].


%Helpers
comma(comma()) --> [','].