% steps_between_pieces(+T, +PieceToMove, +EndCoord, -NumberOfSteps)
% Retorna o numero de passos necessários para uma dada peça chegar a EndCoord.
steps_between_pieces(T, PieceToMove, EndCoord, NumberOfSteps) :-
    PieceToMove esta_em Xi-Yi no_tabuleiro T,
    not(check_all_invalid(T, EndCoord)),
    steps_between_pieces(T, PieceToMove, [Xi-Yi], [Xi-Yi], [0], NumberOfSteps, EndCoord), !.

% steps_between_pieces(+Tabuleiro, +PieceToMove, +Queue, +Visited, +NumberOfStepsList, -TotalNumberOfSteps, +EndCoordinates).
% Predicado auxiliar a steps_between_pieces/4.
steps_between_pieces(T, PieceToMove, [XHead-YHead | Remainder], Visited, [CurrentNumberOfSteps | RestList], TotalNumberOfSteps, EndCoord) :-
    memberchk(XHead-YHead, EndCoord), TotalNumberOfSteps is CurrentNumberOfSteps, !.
steps_between_pieces(T, PieceToMove, [HeadCoord|Remainder], Visited, [CurrentNumberOfSteps | RestList], TotalNumberOfSteps, EndCoord) :-
    setof(Coord,
    ( posso_mover_aux(T, PieceToMove, HeadCoord, Coord),
    not(member(Coord, Visited))), FoundList),

    append(FoundList, Visited, NewVisited),
    append(Remainder, FoundList, NewRemainder),

    NewCurrentNumberOfSteps is CurrentNumberOfSteps + 1,
    length(FoundList, Size),
    build_list(NewCurrentNumberOfSteps, Size, NewCurrentNumberOfStepsList),
    append(RestList, NewCurrentNumberOfStepsList, NewRestList),

    steps_between_pieces(T, PieceToMove, NewRemainder, NewVisited, NewRestList, TotalNumberOfSteps, EndCoord), !.
steps_between_pieces(T, PieceToMove, [HeadCoord|Remainder], Visited, [CurrentNumberOfSteps | RestList], TotalNumberOfSteps, EndCoord) :-
    steps_between_pieces(T, PieceToMove, Remainder, Visited, RestList, TotalNumberOfSteps, EndCoord), !.

% build_list(+X, +N, -List)
% Constroi uma lista de tamanho N, com todos os elementos iguais a X.
build_list(X, N, List)  :- 
    findall(X, between(1, N, _), List).

% check_all_invalid(+T, +ListOfCoords)
% Verifica se todas as posições de ListOfCoords são inválidos.
check_all_invalid(T, []).
check_all_invalid(T, [HeadCoord | Remainder]) :-
    Peca esta_em HeadCoord no_tabuleiro T,
    Peca = -1,
    check_all_invalid(T, Remainder).
check_all_invalid(T, [X-Y | Remainder]) :-
    (X < 1; X > 7; Y < 1; Y > 7),
    check_all_invalid(T, Remainder).

% min(+X, +Y, -Min)
% Retorna o menor entre X e Y.
min(X, Y, X) :- X < Y, !.
min(X, Y, Y) :- X >= Y, !.

% value(+T, +PieceToMove, +ListCoord, -Value)
% Atribui um valor a uma dada peça tendo em conta os movimentos que deve executar a partir da sua posição atual.
value(T, PieceToMove, ListCoord, Value) :-
    steps_between_pieces(T, PieceToMove, ListCoord, NumberOfSteps),
    Value is 1 / NumberOfSteps.
value(T, PieceToMove, ListCoord, 0).

% value(+T, +Equipa, -Value)
% Atribui um valor a um dado tabuleiro, tendo em conta a posição das peças de uma equipa.
value(T, preto, Value) :- 
    not(u esta_em Xu-Yu no_tabuleiro T),
    Value is 100, !.
value(T, preto, Value) :-
    not(n esta_em Xn-Yn no_tabuleiro T),
    Value is -100, !.
value(T, preto, Value) :- 
    n esta_em Xn-Yn no_tabuleiro T,
    valid_moves(T, branco, ListOfMoves),
    % se n esta em movimentos validos, entao o valor deve ser -90
    memberchk([_, Xn-Yn], ListOfMoves),
    % write('n: '), write(Xn-Yn), nl,
    % write(ListOfMoves), nl,
    Value is -90, !.
value(T, preto, Value) :-
    u esta_em Xu-Yu no_tabuleiro T,
    n esta_em Xn-Yn no_tabuleiro T,

    value(T, p, [Xu-Yu], PValue),
    value(T, q, [Xu-Yu], QValue),
    value(T, b, [Xn-Yn], BValue),
    value(T, d, [Xn-Yn], DValue),

    valid_moves(T, branco, ListOfMoves),
    % se p e q estao em movimentos validos, entao o valor o maximo entre os dois deve ser retirado
    ((p esta_em Xp-Yp no_tabuleiro T, q esta_em Xq-Yq no_tabuleiro T,
      memberchk([_, Xp-Yp], ListOfMoves), memberchk([_, Xq-Yq], ListOfMoves), 
      min(PValue, QValue, MinValue), Value is MinValue - BValue - DValue);
    % se apenas p esta em movimentos validos, entao o valor de p deve ser retirado
     (p esta_em Xp-Yp no_tabuleiro T, memberchk([_, Xp-Yp], ListOfMoves), Value is QValue - BValue - DValue);
    % se apenas q esta em movimentos validos, entao o valor de q deve ser retirado
     (q esta_em Xq-Yq no_tabuleiro T, memberchk([_, Xq-Yq], ListOfMoves), Value is PValue - BValue - DValue);
    % se nenhum esta em movimentos validos
     (Value is PValue + QValue - BValue - DValue)), !.
value(T, branco, Value) :- 
    not(n esta_em Xn-Yn no_tabuleiro T),
    Value is 100, !.
value(T, branco, Value) :-
    not(u esta_em Xu-Yu no_tabuleiro T),
    Value is -100, !.
value(T, branco, Value) :- 
    u esta_em Xu-Yu no_tabuleiro T,
    valid_moves(T, preto, ListOfMoves),
    % se n esta em movimentos validos, entao o valor deve ser -90
    memberchk([_, Xu-Yu], ListOfMoves),
    % write('n: '), write(Xn-Yn), nl,
    % write(ListOfMoves), nl,
    Value is -90, !.
value(T, branco, Value) :-
    u esta_em Xu-Yu no_tabuleiro T,
    n esta_em Xn-Yn no_tabuleiro T,

    value(T, p, [Xu-Yu], PValue),
    value(T, q, [Xu-Yu], QValue),
    value(T, b, [Xn-Yn], BValue),
    value(T, d, [Xn-Yn], DValue),

    valid_moves(T, preto, ListOfMoves),
    % se p e d estao em movimentos validos, entao o valor o maximo entre os dois deve ser retirado
    ((b esta_em Xb-Yb no_tabuleiro T, d esta_em Xd-Yd no_tabuleiro T,
      memberchk([_, Xb-Yb], ListOfMoves), memberchk([_, Xd-Yd], ListOfMoves), 
      min(BValue, DValue, MinValue), Value is MinValue - BValue - DValue);
    % se apenas b esta em movimentos validos, entao o valor de b deve ser retirado
     (b esta_em Xb-Yb no_tabuleiro T, memberchk([_, Xb-Yb], ListOfMoves), Value is DValue - PValue - QValue);
    % se apenas d esta em movimentos validos, entao o valor de d deve ser retirado
     (d esta_em Xd-Yd no_tabuleiro T, memberchk([_, Xd-Yd], ListOfMoves), Value is BValue - PValue - QValue);
    % se nenhum esta em movimentos validos
     (Value is BValue + DValue - PValue - QValue)), !.

% verify_suicide(+T, +Equipa, +CFinal)
% Verifica se um movimento de uma dada equipa é suicida.
verify_suicide(T, preto, Xf-Yf) :- not((n esta_em Xf-Yf no_tabuleiro T; p esta_em Xf-Yf no_tabuleiro T; q esta_em Xf-Yf no_tabuleiro T)).
verify_suicide(T, branco, Xf-Yf) :- not((u esta_em Xf-Yf no_tabuleiro T; b esta_em Xf-Yf no_tabuleiro T; d esta_em Xf-Yf no_tabuleiro T)).

% choose_move(+T, +Equipa, +Nivel, -Movimento)
% Escolhe um movimento para uma dada equipa, tendo em conta o nivel de dificuldade.
choose_move(T, Equipa, 1, [Peca, Xf-Yf]):- valid_moves(T, Equipa, L),
                                           random_select(M, L, _R),
                                           nth0(0, M, Peca),
                                           nth0(1, M, Xf-Yf),
                                           verify_suicide(T, Equipa, Xf-Yf).
choose_move(T, Equipa, 1, Move) :- choose_move(T, Equipa, 1, Move).
choose_move(T, Player, 2, Move) :-
    valid_moves(T, Player, ListOfMoves),
    % write(ListOfMoves), nl,
    choose_move_aux(T, Player, ListOfMoves, -101, [], Value, Move).

% choose_move_aux(+T, +Player, +ListOfMoves, +CurrValue, +CurrMove, +Value, -Move)
% Predicado auxiliar a choose_move/4.
choose_move_aux(T, Player, [], Value, Move, Value, Move) :- !.
choose_move_aux(T, Player, [Head | Remainder], CurrValue, CurrMove, Value, Move) :-
    Head = [PieceToMove, X-Y],
    % write(Head), nl,
    move(T, PieceToMove, X, Y, T1),
    % write('Move done'),
    value(T1, Player, CalculatedValue),
    % write('Value: '), write(CalculatedValue), nl,
    CurrValue < CalculatedValue,
    choose_move_aux(T, Player, Remainder, CalculatedValue, Head, Value, Move).
choose_move_aux(T, Player, [Head | Remainder], CurrValue, CurrMove, Value, Move) :-
    choose_move_aux(T, Player, Remainder, CurrValue, CurrMove, Value, Move).
