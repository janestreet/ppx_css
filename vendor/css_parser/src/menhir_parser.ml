
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHITESPACE
    | URI of (
# 29 "menhir_parser.mly"
       (string)
# 12 "menhir_parser.ml"
  )
    | UNICODE_RANGE of (
# 38 "menhir_parser.mly"
       (string)
# 17 "menhir_parser.ml"
  )
    | STRING of (
# 28 "menhir_parser.mly"
       (string)
# 22 "menhir_parser.ml"
  )
    | SEMI_COLON
    | RIGHT_PAREN
    | RIGHT_BRACKET
    | RIGHT_BRACE
    | PERCENTAGE
    | OPERATOR of (
# 30 "menhir_parser.mly"
       (string)
# 32 "menhir_parser.ml"
  )
    | NUMBER of (
# 37 "menhir_parser.mly"
       (string)
# 37 "menhir_parser.ml"
  )
    | NESTED_AT_RULE of (
# 32 "menhir_parser.mly"
       (string)
# 42 "menhir_parser.ml"
  )
    | LEFT_PAREN
    | LEFT_BRACKET
    | LEFT_BRACE
    | IMPORTANT
    | IDENT of (
# 27 "menhir_parser.mly"
       (string)
# 51 "menhir_parser.ml"
  )
    | HASH of (
# 36 "menhir_parser.mly"
       (string)
# 56 "menhir_parser.ml"
  )
    | FUNCTION of (
# 35 "menhir_parser.mly"
       (string)
# 61 "menhir_parser.ml"
  )
    | FLOAT_DIMENSION of (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 66 "menhir_parser.ml"
  )
    | EOF
    | DOT
    | DIMENSION of (
# 40 "menhir_parser.mly"
       (string * string)
# 73 "menhir_parser.ml"
  )
    | DELIM of (
# 31 "menhir_parser.mly"
       (string)
# 78 "menhir_parser.ml"
  )
    | COLON
    | AT_RULE_WITHOUT_BODY of (
# 33 "menhir_parser.mly"
       (string)
# 84 "menhir_parser.ml"
  )
    | AT_RULE of (
# 34 "menhir_parser.mly"
       (string)
# 89 "menhir_parser.ml"
  )
    | AMPERSAND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState133
  | MenhirState123
  | MenhirState121
  | MenhirState107
  | MenhirState106
  | MenhirState100
  | MenhirState96
  | MenhirState90
  | MenhirState86
  | MenhirState84
  | MenhirState79
  | MenhirState72
  | MenhirState66
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState60
  | MenhirState55
  | MenhirState49
  | MenhirState48
  | MenhirState39
  | MenhirState29
  | MenhirState18
  | MenhirState16
  | MenhirState8
  | MenhirState7
  | MenhirState0

# 1 "menhir_parser.mly"
  

(* Workaround for this dune bug: https://github.com/ocaml/dune/issues/2450 *)
module Css = struct end

open Types


# 145 "menhir_parser.ml"

let rec _menhir_goto_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv531) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declarations) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv529) = Obj.magic _menhir_stack in
    let (_endpos_ds_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((ds : 'tv_declarations) : 'tv_declarations) = _v in
    let (_startpos_ds_ : Lexing.position) = _startpos in
    ((let _v : 'tv_declarations_with_loc = let _endpos = _endpos_ds_ in
    let _startpos = _startpos_ds_ in
    
# 112 "menhir_parser.mly"
                      ( (ds, Lex_buffer.make_loc_and_fix ~loc_ghost:true _startpos _endpos) )
# 166 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv527) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declarations_with_loc) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv495 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv491 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv489 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, (xs : 'tv_nested_prelude_with_loc), _startpos_xs_), _endpos__2_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_xs_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_nested_style_rule = let _endpos = _endpos__4_ in
            let _startpos = _startpos_xs_ in
            
# 210 "menhir_parser.mly"
                                                                                      (
      { Style_rule.prelude = xs;
        block = ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 201 "menhir_parser.ml"
             in
            _menhir_goto_nested_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv490)) : 'freshtv492)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv493 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv494)) : 'freshtv496)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv503 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 216 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv499 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 226 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv497 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 234 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 34 "menhir_parser.mly"
       (string)
# 240 "menhir_parser.ml"
            )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__3_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_name_ in
            let _endpos = _endpos__5_ in
            let _v : 'tv_at_rule = let _endpos = _endpos__5_ in
            let _startpos = _startpos_name_ in
            
# 79 "menhir_parser.mly"
                                                                                               (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Declaration_list ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 255 "menhir_parser.ml"
             in
            _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv498)) : 'freshtv500)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv501 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 265 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)) : 'freshtv504)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv511 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv507 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv505 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__2_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_xs_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_style_rule = let _endpos = _endpos__4_ in
            let _startpos = _startpos_xs_ in
            
# 95 "menhir_parser.mly"
                                                                               (
      { Style_rule.prelude = xs;
        block = ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 296 "menhir_parser.ml"
             in
            _menhir_goto_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv506)) : 'freshtv508)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv509 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv510)) : 'freshtv512)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv525 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv521 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv519 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _v : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 321 "menhir_parser.ml"
            ) = 
# 56 "menhir_parser.mly"
                                  ( ds )
# 325 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv517) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 333 "menhir_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv515) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 341 "menhir_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv513) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 349 "menhir_parser.ml"
            )) : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 353 "menhir_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv514)) : 'freshtv516)) : 'freshtv518)) : 'freshtv520)) : 'freshtv522)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv523 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)
    | _ ->
        _menhir_fail ()) : 'freshtv528)) : 'freshtv530)) : 'freshtv532)

and _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv487 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI_COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv483 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState106 in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE_WITHOUT_BODY _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NESTED_AT_RULE _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv481 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_), _endpos__2_, _) = _menhir_stack in
            let _startpos = _startpos_ds_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 119 "menhir_parser.mly"
                                                                                                    ( List.rev ds)
# 458 "menhir_parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv482)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv484)
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState106 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | RIGHT_BRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv485 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_ds_ in
        let _v : 'tv_declarations = 
# 118 "menhir_parser.mly"
                                                                                        ( List.rev ds)
# 480 "menhir_parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv486)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106) : 'freshtv488)

and _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv479 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI_COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE_WITHOUT_BODY _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NESTED_AT_RULE _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv471 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_), _endpos__2_) = _menhir_stack in
            let _startpos = _startpos_ds_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 117 "menhir_parser.mly"
                                                                                            ( List.rev ds )
# 549 "menhir_parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv472)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv474)
    | EOF | RIGHT_BRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv475 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_ds_ in
        let _v : 'tv_declarations = 
# 116 "menhir_parser.mly"
                                                                                ( List.rev ds )
# 565 "menhir_parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv476)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv477 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)) : 'freshtv480)

and _menhir_goto_nested_style_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_nested_style_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv469) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_nested_style_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv467) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : 'tv_nested_style_rule) : 'tv_nested_style_rule) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_nested_rule = 
# 136 "menhir_parser.mly"
                          ( Declaration_list.Style_rule r )
# 595 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv465) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_nested_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((match _menhir_s with
    | MenhirState0 | MenhirState123 | MenhirState96 | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue = 
# 130 "menhir_parser.mly"
                    ( [ r ] )
# 622 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv450)) : 'freshtv452)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv455 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv453 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_), _endpos__2_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue = 
# 131 "menhir_parser.mly"
                                                                                                             ( r :: ds )
# 644 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv454)) : 'freshtv456)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv457) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declaration_or_at_rule = 
# 142 "menhir_parser.mly"
                    ( r )
# 665 "menhir_parser.ml"
         in
        _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv458)) : 'freshtv460)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv463 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue = 
# 132 "menhir_parser.mly"
                                                                                                         ( r :: ds )
# 687 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv462)) : 'freshtv464)
    | _ ->
        _menhir_fail ()) : 'freshtv466)) : 'freshtv468)) : 'freshtv470)

and _menhir_goto_list_component_value_in_nested_prelude_with_loc_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_in_nested_prelude_with_loc_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv443 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 702 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv439 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 712 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv437 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 720 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 35 "menhir_parser.mly"
       (string)
# 726 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_in_nested_prelude_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value_in_nested_prelude = 
# 283 "menhir_parser.mly"
                                                                                     (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_)) )
# 735 "menhir_parser.ml"
             in
            _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv438)) : 'freshtv440)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv441 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 745 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv442)) : 'freshtv444)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * Lexing.position * _menhir_state * 'tv_component_value_in_nested_prelude_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * Lexing.position * _menhir_state * 'tv_component_value_in_nested_prelude_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_in_nested_prelude_with_loc), _startpos_x_), _endpos_xs_, _, (xs : 'tv_list_component_value_in_nested_prelude_with_loc_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_in_nested_prelude_with_loc_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 760 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_in_nested_prelude_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv446)) : 'freshtv448)
    | _ ->
        _menhir_fail ()

and _menhir_goto_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv435 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AT_RULE _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | RIGHT_BRACE ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LEFT_BRACE ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv436)

and _menhir_goto_declaration_or_at_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declaration_or_at_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv421 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv419 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_), _endpos__2_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 124 "menhir_parser.mly"
                                                                                                                        ( d :: ds )
# 841 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv420)) : 'freshtv422)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv425 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_), _endpos__2_, _) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 126 "menhir_parser.mly"
                                                                                                                                ( d :: ds )
# 863 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv424)) : 'freshtv426)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv429 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 125 "menhir_parser.mly"
                                                                                                                    ( d :: ds )
# 885 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv428)) : 'freshtv430)
    | MenhirState0 | MenhirState123 | MenhirState62 | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv431) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_d_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 123 "menhir_parser.mly"
                               ( [d] )
# 906 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv432)) : 'freshtv434)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_IMPORTANT_ : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_boption_IMPORTANT_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv417 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 918 "menhir_parser.ml"
    ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_boption_IMPORTANT_) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv415 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 927 "menhir_parser.ml"
    ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let ((i : 'tv_boption_IMPORTANT_) : 'tv_boption_IMPORTANT_) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let ((((_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 27 "menhir_parser.mly"
       (string)
# 935 "menhir_parser.ml"
    )), _startpos_n_), _, _, _startpos__2_), _endpos__3_, _startpos__3_), _endpos_v_, _, (v : 'tv_list_component_value_with_loc_), _startpos_v_) = _menhir_stack in
    let _startpos = _startpos_n_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_declaration = let _endpos = _endpos_i_ in
    let _startpos = _startpos_n_ in
    
# 146 "menhir_parser.mly"
                                                                                                   (
    { Declaration.name = (n, Lex_buffer.make_loc_and_fix _startpos_n_ _endpos_n_);
      value = (v, Lex_buffer.make_loc_and_fix _startpos_v_ _endpos_v_);
      important = (i, Lex_buffer.make_loc_and_fix _startpos_i_ _endpos_i_);
      loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
    }
  )
# 950 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv413) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv411) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv409) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : 'tv_declaration) : 'tv_declaration) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_declaration_or_at_rule = 
# 140 "menhir_parser.mly"
                    ( Declaration_list.Declaration d )
# 975 "menhir_parser.ml"
     in
    _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv410)) : 'freshtv412)) : 'freshtv414)) : 'freshtv416)) : 'freshtv418)

and _menhir_goto_list_component_value_with_loc_in_nested_prelude_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_in_nested_prelude_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv361 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_nested_bracket_block = 
# 240 "menhir_parser.mly"
                                                                                     ( xs )
# 1003 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv359) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv357) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv355) = Obj.magic _menhir_stack in
            let (_endpos_b_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_nested_bracket_block) : 'tv_nested_bracket_block) = _v in
            let (_startpos_b_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos_b_ in
            let _endpos = _endpos_b_ in
            let _v : 'tv_common_nested_component_value = 
# 261 "menhir_parser.mly"
                             ( Component_value.Bracket_block b )
# 1028 "menhir_parser.ml"
             in
            _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv356)) : 'freshtv358)) : 'freshtv360)) : 'freshtv362)) : 'freshtv364)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv365 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)) : 'freshtv368)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv371 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc_in_nested_prelude)), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_in_nested_prelude_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 1048 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_in_nested_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv370)) : 'freshtv372)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv385 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv381 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv379 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_nested_paren_block = 
# 236 "menhir_parser.mly"
                                                                                 ( xs )
# 1071 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv377) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv375) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv373) = Obj.magic _menhir_stack in
            let (_endpos_b_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_nested_paren_block) : 'tv_nested_paren_block) = _v in
            let (_startpos_b_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos_b_ in
            let _endpos = _endpos_b_ in
            let _v : 'tv_common_nested_component_value = 
# 260 "menhir_parser.mly"
                           ( Component_value.Paren_block b )
# 1096 "menhir_parser.ml"
             in
            _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv374)) : 'freshtv376)) : 'freshtv378)) : 'freshtv380)) : 'freshtv382)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv383 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv405 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_hd_, _menhir_s, (hd : 'tv_starting_component_value_with_loc_in_prelude), _startpos_hd_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
        let _startpos = _startpos_hd_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nested_prelude = 
# 223 "menhir_parser.mly"
                                                                                                             ( hd ::  xs )
# 1117 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv403) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv401) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv399) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_nested_prelude) : 'tv_nested_prelude) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _v : 'tv_nested_prelude_with_loc = let _endpos = _endpos_xs_ in
        let _startpos = _startpos_xs_ in
        
# 219 "menhir_parser.mly"
                      ( (xs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1143 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv397) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv395 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv391 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMPERSAND ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AT_RULE _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AT_RULE_WITHOUT_BODY _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | COLON ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DELIM _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DIMENSION _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DOT ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT_DIMENSION _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | HASH _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_BRACKET ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_PAREN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NESTED_AT_RULE _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NUMBER _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPERATOR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RIGHT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv389 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_menhir_s : _menhir_state) = MenhirState96 in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv387 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__3_ : Lexing.position) = _endpos in
                let (_ : _menhir_state) = _menhir_s in
                ((let ((_menhir_stack, _menhir_s, (xs : 'tv_nested_prelude_with_loc), _startpos_xs_), _endpos__2_) = _menhir_stack in
                let _startpos = _startpos_xs_ in
                let _endpos = _endpos__3_ in
                let _v : 'tv_nested_style_rule = let _endpos = _endpos__3_ in
                let _startpos = _startpos_xs_ in
                
# 204 "menhir_parser.mly"
                                                          (
      { Style_rule.prelude = xs;
        block = [], Location.none;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 1217 "menhir_parser.ml"
                 in
                _menhir_goto_nested_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv388)) : 'freshtv390)
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNICODE_RANGE _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHITESPACE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv392)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv393 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)) : 'freshtv396)) : 'freshtv398)) : 'freshtv400)) : 'freshtv402)) : 'freshtv404)) : 'freshtv406)) : 'freshtv408)
    | _ ->
        _menhir_fail ()

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_in_nested_prelude_with_loc_ = 
# 212 "<standard.mly>"
    ( [] )
# 1247 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_in_nested_prelude_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_component_value_in_nested_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value_in_nested_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState66 | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_component_value_in_nested_prelude) : 'tv_component_value_in_nested_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_in_nested_prelude_with_loc = let _endpos = _endpos_c_ in
        let _startpos = _startpos_c_ in
        
# 277 "menhir_parser.mly"
                                          ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1274 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv341) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_component_value_in_nested_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339 * Lexing.position * _menhir_state * 'tv_component_value_in_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RIGHT_PAREN ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv340)) : 'freshtv342)) : 'freshtv344)) : 'freshtv346)
    | MenhirState90 | MenhirState63 | MenhirState64 | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_component_value_in_nested_prelude) : 'tv_component_value_in_nested_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_with_loc_in_nested_prelude = let _endpos = _endpos_c_ in
        let _startpos = _startpos_c_ in
        
# 227 "menhir_parser.mly"
                                          ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1345 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_component_value_with_loc_in_nested_prelude) = _v in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv347 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACE | RIGHT_BRACKET | RIGHT_PAREN ->
            _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv348)) : 'freshtv350)) : 'freshtv352)) : 'freshtv354)
    | _ ->
        _menhir_fail ()

and _menhir_goto_style_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_style_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv337) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_style_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv335) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : 'tv_style_rule) : 'tv_style_rule) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_rule = 
# 61 "menhir_parser.mly"
                   ( Rule.Style_rule r )
# 1418 "menhir_parser.ml"
     in
    _menhir_goto_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv336)) : 'freshtv338)

and _menhir_goto_at_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_at_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState0 | MenhirState123 | MenhirState62 | MenhirState96 | MenhirState106 | MenhirState107 | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv329) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv327) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_at_rule) : 'tv_at_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declaration_or_at_rule = 
# 141 "menhir_parser.mly"
                ( Declaration_list.At_rule r )
# 1443 "menhir_parser.ml"
         in
        _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv328)) : 'freshtv330)
    | MenhirState133 | MenhirState48 | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv331) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_at_rule) : 'tv_at_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_rule = 
# 60 "menhir_parser.mly"
                ( Rule.At_rule r )
# 1464 "menhir_parser.ml"
         in
        _menhir_goto_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv332)) : 'freshtv334)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_component_value_with_loc_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1479 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv263 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1489 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1497 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 35 "menhir_parser.mly"
       (string)
# 1503 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value = 
# 177 "menhir_parser.mly"
                                                                   (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_))
    )
# 1513 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv262)) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv265 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1523 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv271 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv269 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc), _startpos_x_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 1538 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv270)) : 'freshtv272)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv285 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv283 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_bracket_block = 
# 160 "menhir_parser.mly"
                                                                   ( xs )
# 1561 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv281) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((match _menhir_s with
            | MenhirState86 | MenhirState39 | MenhirState8 | MenhirState16 | MenhirState18 | MenhirState29 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv275) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bracket_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv273) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_bracket_block) : 'tv_bracket_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value = 
# 168 "menhir_parser.mly"
                      ( Component_value.Bracket_block b )
# 1588 "menhir_parser.ml"
                 in
                _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv274)) : 'freshtv276)
            | MenhirState133 | MenhirState7 | MenhirState48 | MenhirState121 | MenhirState60 | MenhirState49 | MenhirState55 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv279) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bracket_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv277) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_bracket_block) : 'tv_bracket_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value_in_prelude = 
# 195 "menhir_parser.mly"
                      ( Component_value.Bracket_block b )
# 1609 "menhir_parser.ml"
                 in
                _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv278)) : 'freshtv280)
            | _ ->
                _menhir_fail ()) : 'freshtv282)) : 'freshtv284)) : 'freshtv286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv287 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)) : 'freshtv290)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv303 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_paren_block = 
# 156 "menhir_parser.mly"
                                                               ( xs )
# 1641 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv299) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((match _menhir_s with
            | MenhirState86 | MenhirState39 | MenhirState8 | MenhirState16 | MenhirState29 | MenhirState18 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_paren_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_paren_block) : 'tv_paren_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value = 
# 167 "menhir_parser.mly"
                    ( Component_value.Paren_block b )
# 1668 "menhir_parser.ml"
                 in
                _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv292)) : 'freshtv294)
            | MenhirState133 | MenhirState7 | MenhirState48 | MenhirState121 | MenhirState60 | MenhirState55 | MenhirState49 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv297) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_paren_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv295) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_paren_block) : 'tv_paren_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value_in_prelude = 
# 194 "menhir_parser.mly"
                    ( Component_value.Paren_block b )
# 1689 "menhir_parser.ml"
                 in
                _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv296)) : 'freshtv298)
            | _ ->
                _menhir_fail ()) : 'freshtv300)) : 'freshtv302)) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv305 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv315 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1706 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv311 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1716 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv309 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1724 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 35 "menhir_parser.mly"
       (string)
# 1730 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value_in_prelude = 
# 197 "menhir_parser.mly"
                                                                   (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_)) )
# 1739 "menhir_parser.ml"
             in
            _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv310)) : 'freshtv312)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv313 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1749 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)) : 'freshtv316)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv325 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1758 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IMPORTANT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv319) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv317) = Obj.magic _menhir_stack in
            let (_endpos__1_ : Lexing.position) = _endpos in
            let (_startpos__1_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos__1_ in
            let _endpos = _endpos__1_ in
            let _v : 'tv_boption_IMPORTANT_ = 
# 136 "<standard.mly>"
    ( true )
# 1778 "menhir_parser.ml"
             in
            _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv318)) : 'freshtv320)
        | EOF | RIGHT_BRACE | SEMI_COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv321) = Obj.magic _menhir_stack in
            ((let (_, _startpos) = Obj.magic _menhir_stack in
            let _endpos = _startpos in
            let _v : 'tv_boption_IMPORTANT_ = 
# 134 "<standard.mly>"
    ( false )
# 1789 "menhir_parser.ml"
             in
            _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv322)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv323 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1799 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)) : 'freshtv326)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_WHITESPACE_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_WHITESPACE_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState86 | MenhirState39 | MenhirState8 | MenhirState16 | MenhirState29 | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_component_value = 
# 175 "menhir_parser.mly"
                              ( Component_value.Delim ":" )
# 1832 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv240)) : 'freshtv242)
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_component_value = 
# 176 "menhir_parser.mly"
                            ( Component_value.Delim "." )
# 1851 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv244)) : 'freshtv246)
        | HASH _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 36 "menhir_parser.mly"
       (string)
# 1861 "menhir_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos_h_ : Lexing.position) = _endpos in
            let ((h : (
# 36 "menhir_parser.mly"
       (string)
# 1871 "menhir_parser.ml"
            )) : (
# 36 "menhir_parser.mly"
       (string)
# 1875 "menhir_parser.ml"
            )) = _v in
            let (_startpos_h_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_h_ in
            let _v : 'tv_component_value = 
# 181 "menhir_parser.mly"
                                 ( Component_value.Hash h )
# 1884 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv248)) : 'freshtv250)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv259 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1899 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1909 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DELIM _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DIMENSION _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT_DIMENSION _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FUNCTION _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_BRACKET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_PAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NUMBER _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPERATOR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNICODE_RANGE _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | URI _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHITESPACE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | COLON | DOT | HASH _ ->
                _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | EOF | IMPORTANT | RIGHT_BRACE | SEMI_COLON ->
                _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState86
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv256)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1958 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)) : 'freshtv260)
    | _ ->
        _menhir_fail ()

and _menhir_goto_component_value : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv237) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv235) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_component_value) : 'tv_component_value) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_component_value_with_loc = let _endpos = _endpos_c_ in
    let _startpos = _startpos_c_ in
    
# 164 "menhir_parser.mly"
                        ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1986 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv233) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv231 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | EOF | IMPORTANT | RIGHT_BRACE | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv232)) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_list_component_value_with_loc_in_nested_prelude_ = 
# 212 "<standard.mly>"
    ( [] )
# 2046 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_in_nested_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 2053 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv229) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 27 "menhir_parser.mly"
       (string)
# 2064 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 2068 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value_in_nested_prelude = 
# 282 "menhir_parser.mly"
              ( Component_value.Ident i )
# 2076 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv230)

and _menhir_run66 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 2083 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHT_PAREN ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_goto_common_nested_component_value : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_common_nested_component_value -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState90 | MenhirState63 | MenhirState64 | MenhirState79 | MenhirState66 | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_nested_component_value) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_nested_component_value) : 'tv_common_nested_component_value) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_in_nested_prelude = 
# 281 "menhir_parser.mly"
                                      ( c )
# 2150 "menhir_parser.ml"
         in
        _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv212)) : 'freshtv214)
    | MenhirState0 | MenhirState123 | MenhirState62 | MenhirState96 | MenhirState106 | MenhirState107 | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv227) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_nested_component_value) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_nested_component_value) : 'tv_common_nested_component_value) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_starting_component_value_in_nested_prelude = 
# 272 "menhir_parser.mly"
                                      ( c )
# 2171 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_starting_component_value_in_nested_prelude) : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_starting_component_value_with_loc_in_prelude = let _endpos = _endpos_c_ in
        let _startpos = _startpos_c_ in
        
# 231 "menhir_parser.mly"
                                                   ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2198 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_with_loc_in_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACE ->
            _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv216)) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)) : 'freshtv226)) : 'freshtv228)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_component_value_with_loc_in_prelude_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_in_prelude_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState133 | MenhirState7 | MenhirState48 | MenhirState121 | MenhirState60 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_component_value_with_loc_in_prelude_) : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_prelude = 
# 108 "menhir_parser.mly"
                                                 ( xs )
# 2274 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_prelude) : 'tv_prelude) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _v : 'tv_prelude_with_loc = let _endpos = _endpos_xs_ in
        let _startpos = _startpos_xs_ in
        
# 104 "menhir_parser.mly"
               ( (xs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2300 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        match _menhir_s with
        | MenhirState7 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv169 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2314 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv165 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2324 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AT_RULE _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DELIM _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DIMENSION _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DOT ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT_DIMENSION _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FUNCTION _v ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_BRACKET ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_PAREN ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NUMBER _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OPERATOR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UNICODE_RANGE _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHITESPACE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHT_BRACE ->
                    _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | LEFT_BRACE ->
                    _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv166)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv167 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2382 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)) : 'freshtv170)
        | MenhirState49 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv177 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2391 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI_COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv173 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2401 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv171 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2409 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__3_ : Lexing.position) = _endpos in
                ((let ((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 33 "menhir_parser.mly"
       (string)
# 2415 "menhir_parser.ml"
                )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_) = _menhir_stack in
                let _startpos = _startpos_name_ in
                let _endpos = _endpos__3_ in
                let _v : 'tv_at_rule = let _endpos = _endpos__3_ in
                let _startpos = _startpos_name_ in
                
# 65 "menhir_parser.mly"
                                                                   (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Empty;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 2430 "menhir_parser.ml"
                 in
                _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv172)) : 'freshtv174)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv175 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2440 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)
        | MenhirState60 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv183 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2449 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv179 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2459 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AMPERSAND ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DELIM _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DIMENSION _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DOT ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT_DIMENSION _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_BRACKET ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_PAREN ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NUMBER _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OPERATOR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UNICODE_RANGE _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHITESPACE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv180)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv181 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2513 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv182)) : 'freshtv184)
        | MenhirState133 | MenhirState48 | MenhirState121 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv193 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AMPERSAND ->
                    _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DELIM _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DIMENSION _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DOT ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT_DIMENSION _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_BRACKET ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_PAREN ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NUMBER _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OPERATOR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHT_BRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv187 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_menhir_s : _menhir_state) = MenhirState123 in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv185 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos__3_ : Lexing.position) = _endpos in
                    let (_ : _menhir_state) = _menhir_s in
                    ((let ((_menhir_stack, _menhir_s, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__2_) = _menhir_stack in
                    let _startpos = _startpos_xs_ in
                    let _endpos = _endpos__3_ in
                    let _v : 'tv_style_rule = let _endpos = _endpos__3_ in
                    let _startpos = _startpos_xs_ in
                    
# 89 "menhir_parser.mly"
                                                   (
      { Style_rule.prelude = xs;
        block = [], Location.none;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 2584 "menhir_parser.ml"
                     in
                    _menhir_goto_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv186)) : 'freshtv188)
                | STRING _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UNICODE_RANGE _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHITESPACE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123) : 'freshtv190)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv191 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv192)) : 'freshtv194)
        | _ ->
            _menhir_fail ()) : 'freshtv196)) : 'freshtv198)) : 'freshtv200)) : 'freshtv202)) : 'freshtv204)) : 'freshtv206)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_component_value_with_loc_in_prelude_) : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc_in_prelude), _startpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_in_prelude_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 2625 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_in_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv208)) : 'freshtv210)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_rule_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_rule_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rule_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_rule_) : 'tv_list_rule_) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_rule), _startpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_rule_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 2653 "menhir_parser.ml"
         in
        _menhir_goto_list_rule_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv134)) : 'freshtv136)
    | MenhirState133 | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rule_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161) = Obj.magic _menhir_stack in
        let (_endpos_rs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((rs : 'tv_list_rule_) : 'tv_list_rule_) = _v in
        let (_startpos_rs_ : Lexing.position) = _startpos in
        ((let _v : 'tv_stylesheet_without_eof = let _endpos = _endpos_rs_ in
        let _startpos = _startpos_rs_ in
        
# 52 "menhir_parser.mly"
                  ( (rs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2674 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_stylesheet_without_eof) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState48 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv143 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2687 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv139 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2697 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv137 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2705 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                let (_endpos__5_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 32 "menhir_parser.mly"
       (string)
# 2711 "menhir_parser.ml"
                )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__3_), _, (s : 'tv_stylesheet_without_eof)) = _menhir_stack in
                let _startpos = _startpos_name_ in
                let _endpos = _endpos__5_ in
                let _v : 'tv_at_rule = let _endpos = _endpos__5_ in
                let _startpos = _startpos_name_ in
                
# 72 "menhir_parser.mly"
                                                                                                      (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Stylesheet s;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 2726 "menhir_parser.ml"
                 in
                _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv138)) : 'freshtv140)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv141 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2736 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)
        | MenhirState133 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EOF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (s : 'tv_stylesheet_without_eof)) = _menhir_stack in
                let _v : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2755 "menhir_parser.ml"
                ) = 
# 48 "menhir_parser.mly"
                                  ( s )
# 2759 "menhir_parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv149) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2767 "menhir_parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv147) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2775 "menhir_parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv145) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2783 "menhir_parser.ml"
                )) : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2787 "menhir_parser.ml"
                )) = _v in
                (Obj.magic _1 : 'freshtv146)) : 'freshtv148)) : 'freshtv150)) : 'freshtv152)) : 'freshtv154)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv155 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)) : 'freshtv158)
        | _ ->
            _menhir_fail ()) : 'freshtv160)) : 'freshtv162)) : 'freshtv164)
    | _ ->
        _menhir_fail ()

and _menhir_goto_component_value_in_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value_in_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv131) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_in_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_component_value_in_prelude) : 'tv_component_value_in_prelude) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_component_value_with_loc_in_prelude = let _endpos = _endpos_c_ in
    let _startpos = _startpos_c_ in
    
# 190 "menhir_parser.mly"
                                   ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2823 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv127) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc_in_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState55 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE | SEMI_COLON ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv126)) : 'freshtv128)) : 'freshtv130)) : 'freshtv132)

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_with_loc_ = 
# 212 "<standard.mly>"
    ( [] )
# 2881 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : 'tv_option_WHITESPACE_ = 
# 115 "<standard.mly>"
    ( None )
# 2891 "menhir_parser.ml"
     in
    _menhir_goto_option_WHITESPACE_ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run9 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_endpos_x_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos_x_ : Lexing.position) = _startpos in
    ((let x = () in
    let _startpos = _startpos_x_ in
    let _v : 'tv_option_WHITESPACE_ = 
# 117 "<standard.mly>"
    ( Some x )
# 2908 "menhir_parser.ml"
     in
    _menhir_goto_option_WHITESPACE_ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv124)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 29 "menhir_parser.mly"
       (string)
# 2915 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_endpos_u_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((u : (
# 29 "menhir_parser.mly"
       (string)
# 2926 "menhir_parser.ml"
    )) : (
# 29 "menhir_parser.mly"
       (string)
# 2930 "menhir_parser.ml"
    )) = _v in
    let (_startpos_u_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_u_ in
    let _endpos = _endpos_u_ in
    let _v : 'tv_component_value = 
# 172 "menhir_parser.mly"
            ( Component_value.Uri u )
# 2938 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv122)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 38 "menhir_parser.mly"
       (string)
# 2945 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : (
# 38 "menhir_parser.mly"
       (string)
# 2956 "menhir_parser.ml"
    )) : (
# 38 "menhir_parser.mly"
       (string)
# 2960 "menhir_parser.ml"
    )) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_component_value = 
# 183 "menhir_parser.mly"
                      ( Component_value.Unicode_range r )
# 2968 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv120)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 28 "menhir_parser.mly"
       (string)
# 2975 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_endpos_s_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 28 "menhir_parser.mly"
       (string)
# 2986 "menhir_parser.ml"
    )) : (
# 28 "menhir_parser.mly"
       (string)
# 2990 "menhir_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_component_value = 
# 171 "menhir_parser.mly"
               ( Component_value.String s )
# 2998 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv118)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 30 "menhir_parser.mly"
       (string)
# 3005 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
    let (_endpos_o_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((o : (
# 30 "menhir_parser.mly"
       (string)
# 3016 "menhir_parser.ml"
    )) : (
# 30 "menhir_parser.mly"
       (string)
# 3020 "menhir_parser.ml"
    )) = _v in
    let (_startpos_o_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_o_ in
    let _endpos = _endpos_o_ in
    let _v : 'tv_component_value = 
# 173 "menhir_parser.mly"
                 ( Component_value.Operator o )
# 3028 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv116)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 37 "menhir_parser.mly"
       (string)
# 3035 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PERCENTAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3047 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3055 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 37 "menhir_parser.mly"
       (string)
# 3061 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_component_value = 
# 169 "menhir_parser.mly"
                           ( Component_value.Percentage n )
# 3068 "menhir_parser.ml"
         in
        _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv108)) : 'freshtv110)
    | COLON | DELIM _ | DIMENSION _ | DOT | EOF | FLOAT_DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT | LEFT_BRACKET | LEFT_PAREN | NUMBER _ | OPERATOR _ | RIGHT_BRACE | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON | STRING _ | UNICODE_RANGE _ | URI _ | WHITESPACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3076 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 37 "menhir_parser.mly"
       (string)
# 3081 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_component_value = 
# 182 "menhir_parser.mly"
               ( Component_value.Number n )
# 3088 "menhir_parser.ml"
         in
        _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv112)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3098 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 3106 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv105) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 27 "menhir_parser.mly"
       (string)
# 3117 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 3121 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value = 
# 170 "menhir_parser.mly"
              ( Component_value.Ident i )
# 3129 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv106)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 3136 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | RIGHT_PAREN ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 3181 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv103) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 3192 "menhir_parser.ml"
    )) : (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 3196 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 184 "menhir_parser.mly"
                        ( Component_value.Float_dimension d )
# 3204 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv104)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 40 "menhir_parser.mly"
       (string * string)
# 3211 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 40 "menhir_parser.mly"
       (string * string)
# 3222 "menhir_parser.ml"
    )) : (
# 40 "menhir_parser.mly"
       (string * string)
# 3226 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 185 "menhir_parser.mly"
                  ( Component_value.Dimension d )
# 3234 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv102)

and _menhir_run21 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 31 "menhir_parser.mly"
       (string)
# 3241 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 31 "menhir_parser.mly"
       (string)
# 3252 "menhir_parser.ml"
    )) : (
# 31 "menhir_parser.mly"
       (string)
# 3256 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 174 "menhir_parser.mly"
              ( Component_value.Delim d )
# 3264 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv100)

and _menhir_goto_common_component_values_in_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_common_component_values_in_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState133 | MenhirState7 | MenhirState48 | MenhirState121 | MenhirState60 | MenhirState49 | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_component_values_in_prelude) : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_in_prelude = 
# 196 "menhir_parser.mly"
                                           ( c )
# 3289 "menhir_parser.ml"
         in
        _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv92)) : 'freshtv94)
    | MenhirState0 | MenhirState123 | MenhirState62 | MenhirState96 | MenhirState106 | MenhirState107 | MenhirState100 | MenhirState90 | MenhirState63 | MenhirState64 | MenhirState79 | MenhirState66 | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_component_values_in_prelude) : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_common_nested_component_value = 
# 259 "menhir_parser.mly"
                                           ( c )
# 3310 "menhir_parser.ml"
         in
        _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv96)) : 'freshtv98)
    | _ ->
        _menhir_fail ()

and _menhir_run63 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHT_PAREN ->
        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run64 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHT_BRACKET ->
        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run84 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 3409 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WHITESPACE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run67 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_common_nested_component_value = 
# 262 "menhir_parser.mly"
              ( Component_value.Ampersand )
# 3438 "menhir_parser.ml"
     in
    _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv90)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState133 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv36)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState106 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 3489 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 3498 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * Lexing.position * _menhir_state * 'tv_component_value_in_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 3517 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv65 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 3536 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 3545 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 3559 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv73 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 3568 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 3577 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 3591 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 3610 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv88)

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_with_loc_in_prelude_ = 
# 212 "<standard.mly>"
    ( [] )
# 3626 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_in_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_rule_ = 
# 212 "<standard.mly>"
    ( [] )
# 3637 "menhir_parser.ml"
     in
    _menhir_goto_list_rule_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv33) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 248 "menhir_parser.mly"
               ( Component_value.Delim "*" )
# 3654 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv34)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 38 "menhir_parser.mly"
       (string)
# 3661 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv31) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : (
# 38 "menhir_parser.mly"
       (string)
# 3672 "menhir_parser.ml"
    )) : (
# 38 "menhir_parser.mly"
       (string)
# 3676 "menhir_parser.ml"
    )) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 253 "menhir_parser.mly"
                      ( Component_value.Unicode_range r )
# 3684 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv32)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 28 "menhir_parser.mly"
       (string)
# 3691 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
    let (_endpos_s_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 28 "menhir_parser.mly"
       (string)
# 3702 "menhir_parser.ml"
    )) : (
# 28 "menhir_parser.mly"
       (string)
# 3706 "menhir_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 245 "menhir_parser.mly"
               ( Component_value.String s )
# 3714 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv30)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 30 "menhir_parser.mly"
       (string)
# 3721 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
    let (_endpos_o_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((o : (
# 30 "menhir_parser.mly"
       (string)
# 3732 "menhir_parser.ml"
    )) : (
# 30 "menhir_parser.mly"
       (string)
# 3736 "menhir_parser.ml"
    )) = _v in
    let (_startpos_o_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_o_ in
    let _endpos = _endpos_o_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 246 "menhir_parser.mly"
                 ( Component_value.Operator o )
# 3744 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv28)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 37 "menhir_parser.mly"
       (string)
# 3751 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PERCENTAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3763 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3771 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 37 "menhir_parser.mly"
       (string)
# 3777 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_common_component_values_in_prelude = 
# 244 "menhir_parser.mly"
                           ( Component_value.Percentage n )
# 3784 "menhir_parser.ml"
         in
        _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv20)) : 'freshtv22)
    | AMPERSAND | COLON | DELIM _ | DIMENSION _ | DOT | FLOAT_DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | LEFT_BRACE | LEFT_BRACKET | LEFT_PAREN | NUMBER _ | OPERATOR _ | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON | STRING _ | UNICODE_RANGE _ | WHITESPACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3792 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 37 "menhir_parser.mly"
       (string)
# 3797 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_common_component_values_in_prelude = 
# 252 "menhir_parser.mly"
               ( Component_value.Number n )
# 3804 "menhir_parser.ml"
         in
        _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3814 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 32 "menhir_parser.mly"
       (string)
# 3822 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | RIGHT_PAREN ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run16 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | RIGHT_BRACKET ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run37 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 3951 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 27 "menhir_parser.mly"
       (string)
# 3962 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 3966 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value_in_prelude = 
# 200 "menhir_parser.mly"
              ( Component_value.Ident i )
# 3974 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv18)

and _menhir_run38 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 36 "menhir_parser.mly"
       (string)
# 3981 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_endpos_h_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((h : (
# 36 "menhir_parser.mly"
       (string)
# 3992 "menhir_parser.ml"
    )) : (
# 36 "menhir_parser.mly"
       (string)
# 3996 "menhir_parser.ml"
    )) = _v in
    let (_startpos_h_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_h_ in
    let _endpos = _endpos_h_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 251 "menhir_parser.mly"
             ( Component_value.Hash h )
# 4004 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv16)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 4011 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState39 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce86 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | RIGHT_PAREN ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run42 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 4056 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 4067 "menhir_parser.ml"
    )) : (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 4071 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 254 "menhir_parser.mly"
                        ( Component_value.Float_dimension d )
# 4079 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv14)

and _menhir_run43 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 250 "menhir_parser.mly"
        ( Component_value.Delim "." )
# 4096 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv12)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 40 "menhir_parser.mly"
       (string * string)
# 4103 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 40 "menhir_parser.mly"
       (string * string)
# 4114 "menhir_parser.ml"
    )) : (
# 40 "menhir_parser.mly"
       (string * string)
# 4118 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 255 "menhir_parser.mly"
                  ( Component_value.Dimension d )
# 4126 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv10)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 31 "menhir_parser.mly"
       (string)
# 4133 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 31 "menhir_parser.mly"
       (string)
# 4144 "menhir_parser.ml"
    )) : (
# 31 "menhir_parser.mly"
       (string)
# 4148 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 247 "menhir_parser.mly"
              ( Component_value.Delim d )
# 4156 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv8)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 249 "menhir_parser.mly"
          ( Component_value.Delim ":" )
# 4173 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv6)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 33 "menhir_parser.mly"
       (string)
# 4180 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI_COLON ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run60 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "menhir_parser.mly"
       (string)
# 4227 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and declaration_list : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 4286 "menhir_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv4))

and stylesheet : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 4344 "menhir_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AT_RULE _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF ->
        _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | LEFT_BRACE ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133) : 'freshtv2))

# 270 "<standard.mly>"
  

# 4406 "menhir_parser.ml"
