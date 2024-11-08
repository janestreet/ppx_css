
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
# 39 "menhir_parser.mly"
       (string)
# 17 "menhir_parser.ml"
  )
    | STRING of (
# 28 "menhir_parser.mly"
       (string)
# 22 "menhir_parser.ml"
  )
    | SEMI_COLON
    | SELECTOR_FUNCTION of (
# 36 "menhir_parser.mly"
       (string)
# 28 "menhir_parser.ml"
  )
    | RIGHT_PAREN
    | RIGHT_BRACKET
    | RIGHT_BRACE
    | PERCENTAGE
    | OPERATOR of (
# 30 "menhir_parser.mly"
       (string)
# 37 "menhir_parser.ml"
  )
    | NUMBER of (
# 38 "menhir_parser.mly"
       (string)
# 42 "menhir_parser.ml"
  )
    | NESTED_AT_RULE of (
# 32 "menhir_parser.mly"
       (string)
# 47 "menhir_parser.ml"
  )
    | LEFT_PAREN
    | LEFT_BRACKET
    | LEFT_BRACE
    | IMPORTANT
    | IDENT of (
# 27 "menhir_parser.mly"
       (string)
# 56 "menhir_parser.ml"
  )
    | HASH of (
# 37 "menhir_parser.mly"
       (string)
# 61 "menhir_parser.ml"
  )
    | FUNCTION of (
# 35 "menhir_parser.mly"
       (string)
# 66 "menhir_parser.ml"
  )
    | FLOAT_DIMENSION of (
# 40 "menhir_parser.mly"
       (string * string * Types.dimension)
# 71 "menhir_parser.ml"
  )
    | EOF
    | DOT
    | DIMENSION of (
# 41 "menhir_parser.mly"
       (string * string)
# 78 "menhir_parser.ml"
  )
    | DELIM of (
# 31 "menhir_parser.mly"
       (string)
# 83 "menhir_parser.ml"
  )
    | COLON
    | AT_RULE_WITHOUT_BODY of (
# 33 "menhir_parser.mly"
       (string)
# 89 "menhir_parser.ml"
  )
    | AT_RULE of (
# 34 "menhir_parser.mly"
       (string)
# 94 "menhir_parser.ml"
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
  | MenhirState142
  | MenhirState132
  | MenhirState130
  | MenhirState116
  | MenhirState115
  | MenhirState109
  | MenhirState105
  | MenhirState99
  | MenhirState95
  | MenhirState93
  | MenhirState86
  | MenhirState79
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState66
  | MenhirState61
  | MenhirState60
  | MenhirState54
  | MenhirState43
  | MenhirState31
  | MenhirState20
  | MenhirState18
  | MenhirState14
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState0

# 1 "menhir_parser.mly"
  

(* Workaround for this dune bug: https://github.com/ocaml/dune/issues/2450 *)
module Css = struct end

open Types


# 153 "menhir_parser.ml"

let rec _menhir_goto_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv561) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declarations) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv559) = Obj.magic _menhir_stack in
    let (_endpos_ds_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((ds : 'tv_declarations) : 'tv_declarations) = _v in
    let (_startpos_ds_ : Lexing.position) = _startpos in
    ((let _v : 'tv_declarations_with_loc = let _endpos = _endpos_ds_ in
    let _startpos = _startpos_ds_ in
    
# 113 "menhir_parser.mly"
                      ( (ds, Lex_buffer.make_loc_and_fix ~loc_ghost:true _startpos _endpos) )
# 174 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv557) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declarations_with_loc) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv525 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv521 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv519 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, (xs : 'tv_nested_prelude_with_loc), _startpos_xs_), _endpos__2_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_xs_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_nested_style_rule = let _endpos = _endpos__4_ in
            let _startpos = _startpos_xs_ in
            
# 232 "menhir_parser.mly"
                                                                                      (
      { Style_rule.prelude = xs;
        block = ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 209 "menhir_parser.ml"
             in
            _menhir_goto_nested_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv520)) : 'freshtv522)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv523 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv533 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 224 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv529 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 234 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv527 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 242 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 34 "menhir_parser.mly"
       (string)
# 248 "menhir_parser.ml"
            )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__3_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_name_ in
            let _endpos = _endpos__5_ in
            let _v : 'tv_at_rule = let _endpos = _endpos__5_ in
            let _startpos = _startpos_name_ in
            
# 80 "menhir_parser.mly"
                                                                                               (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Declaration_list ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 263 "menhir_parser.ml"
             in
            _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv528)) : 'freshtv530)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv531 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 273 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv532)) : 'freshtv534)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv541 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv537 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv535 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__2_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_xs_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_style_rule = let _endpos = _endpos__4_ in
            let _startpos = _startpos_xs_ in
            
# 96 "menhir_parser.mly"
                                                                               (
      { Style_rule.prelude = xs;
        block = ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 304 "menhir_parser.ml"
             in
            _menhir_goto_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv536)) : 'freshtv538)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv539 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv540)) : 'freshtv542)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv555 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv551 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv549 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _v : (
# 44 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 329 "menhir_parser.ml"
            ) = 
# 57 "menhir_parser.mly"
                                  ( ds )
# 333 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv547) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 44 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 341 "menhir_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv545) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 44 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 349 "menhir_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv543) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 44 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 357 "menhir_parser.ml"
            )) : (
# 44 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 361 "menhir_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv544)) : 'freshtv546)) : 'freshtv548)) : 'freshtv550)) : 'freshtv552)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv553 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv554)) : 'freshtv556)
    | _ ->
        _menhir_fail ()) : 'freshtv558)) : 'freshtv560)) : 'freshtv562)

and _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv517 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI_COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv513 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState115 in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE_WITHOUT_BODY _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NESTED_AT_RULE _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv511 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_), _endpos__2_, _) = _menhir_stack in
            let _startpos = _startpos_ds_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 120 "menhir_parser.mly"
                                                                                                    ( List.rev ds)
# 466 "menhir_parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv512)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv514)
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | RIGHT_BRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv515 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_ds_ in
        let _v : 'tv_declarations = 
# 119 "menhir_parser.mly"
                                                                                        ( List.rev ds)
# 488 "menhir_parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv516)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv518)

and _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv509 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI_COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv503 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE _v ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE_WITHOUT_BODY _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NESTED_AT_RULE _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv501 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_), _endpos__2_) = _menhir_stack in
            let _startpos = _startpos_ds_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 118 "menhir_parser.mly"
                                                                                            ( List.rev ds )
# 557 "menhir_parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv502)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv504)
    | EOF | RIGHT_BRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv505 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_ds_ in
        let _v : 'tv_declarations = 
# 117 "menhir_parser.mly"
                                                                                ( List.rev ds )
# 573 "menhir_parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv506)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv507 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv508)) : 'freshtv510)

and _menhir_goto_nested_style_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_nested_style_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv499) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_nested_style_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv497) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : 'tv_nested_style_rule) : 'tv_nested_style_rule) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_nested_rule = 
# 137 "menhir_parser.mly"
                          ( Declaration_list.Style_rule r )
# 603 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv495) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_nested_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((match _menhir_s with
    | MenhirState0 | MenhirState132 | MenhirState105 | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue = 
# 131 "menhir_parser.mly"
                    ( [ r ] )
# 630 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv480)) : 'freshtv482)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv485 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv483 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_), _endpos__2_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue = 
# 132 "menhir_parser.mly"
                                                                                                             ( r :: ds )
# 652 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv484)) : 'freshtv486)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv489) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv487) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declaration_or_at_rule = 
# 143 "menhir_parser.mly"
                    ( r )
# 673 "menhir_parser.ml"
         in
        _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv488)) : 'freshtv490)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv491 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue = 
# 133 "menhir_parser.mly"
                                                                                                         ( r :: ds )
# 695 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv492)) : 'freshtv494)
    | _ ->
        _menhir_fail ()) : 'freshtv496)) : 'freshtv498)) : 'freshtv500)

and _menhir_goto_list_component_value_in_nested_prelude_with_loc_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_in_nested_prelude_with_loc_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv465 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 710 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv461 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 720 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv459 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 728 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 35 "menhir_parser.mly"
       (string)
# 734 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_in_nested_prelude_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value_in_nested_prelude = 
# 308 "menhir_parser.mly"
                                                                                     (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_)) )
# 743 "menhir_parser.ml"
             in
            _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv460)) : 'freshtv462)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv463 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 753 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)) : 'freshtv466)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * Lexing.position * _menhir_state * 'tv_component_value_in_nested_prelude_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * Lexing.position * _menhir_state * 'tv_component_value_in_nested_prelude_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_in_nested_prelude_with_loc), _startpos_x_), _endpos_xs_, _, (xs : 'tv_list_component_value_in_nested_prelude_with_loc_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_in_nested_prelude_with_loc_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 768 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_in_nested_prelude_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv468)) : 'freshtv470)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv477 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 776 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv473 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 786 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv471 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 794 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 36 "menhir_parser.mly"
       (string)
# 800 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_in_nested_prelude_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value_in_nested_prelude = 
# 305 "menhir_parser.mly"
                                                                                              (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_)) )
# 809 "menhir_parser.ml"
             in
            _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv472)) : 'freshtv474)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv475 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 819 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_in_nested_prelude_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv476)) : 'freshtv478)
    | _ ->
        _menhir_fail ()

and _menhir_goto_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv457 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AT_RULE _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | RIGHT_BRACE ->
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | LEFT_BRACE ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState130
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130) : 'freshtv458)

and _menhir_goto_declaration_or_at_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declaration_or_at_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv443 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_), _endpos__2_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 125 "menhir_parser.mly"
                                                                                                                        ( d :: ds )
# 903 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv442)) : 'freshtv444)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_), _endpos__2_, _) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 127 "menhir_parser.mly"
                                                                                                                                ( d :: ds )
# 925 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv446)) : 'freshtv448)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv449 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 126 "menhir_parser.mly"
                                                                                                                    ( d :: ds )
# 947 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv450)) : 'freshtv452)
    | MenhirState0 | MenhirState132 | MenhirState68 | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv455) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_d_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 124 "menhir_parser.mly"
                               ( [d] )
# 968 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv454)) : 'freshtv456)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_IMPORTANT_ : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_boption_IMPORTANT_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv439 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 980 "menhir_parser.ml"
    ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_boption_IMPORTANT_) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv437 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 989 "menhir_parser.ml"
    ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let ((i : 'tv_boption_IMPORTANT_) : 'tv_boption_IMPORTANT_) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let ((((_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 27 "menhir_parser.mly"
       (string)
# 997 "menhir_parser.ml"
    )), _startpos_n_), _, _, _startpos__2_), _endpos__3_, _startpos__3_), _endpos_v_, _, (v : 'tv_list_component_value_with_loc_), _startpos_v_) = _menhir_stack in
    let _startpos = _startpos_n_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_declaration = let _endpos = _endpos_i_ in
    let _startpos = _startpos_n_ in
    
# 147 "menhir_parser.mly"
                                                                                                   (
    { Declaration.name = (n, Lex_buffer.make_loc_and_fix _startpos_n_ _endpos_n_);
      value = (v, Lex_buffer.make_loc_and_fix _startpos_v_ _endpos_v_);
      important = (i, Lex_buffer.make_loc_and_fix _startpos_i_ _endpos_i_);
      loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
    }
  )
# 1012 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv435) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv431) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : 'tv_declaration) : 'tv_declaration) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_declaration_or_at_rule = 
# 141 "menhir_parser.mly"
                    ( Declaration_list.Declaration d )
# 1037 "menhir_parser.ml"
     in
    _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv432)) : 'freshtv434)) : 'freshtv436)) : 'freshtv438)) : 'freshtv440)

and _menhir_goto_list_component_value_with_loc_in_nested_prelude_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_in_nested_prelude_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv389 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv385 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv383 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_nested_bracket_block = 
# 262 "menhir_parser.mly"
                                                                                     ( xs )
# 1065 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv381) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv379) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv377) = Obj.magic _menhir_stack in
            let (_endpos_b_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_nested_bracket_block) : 'tv_nested_bracket_block) = _v in
            let (_startpos_b_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos_b_ in
            let _endpos = _endpos_b_ in
            let _v : 'tv_common_nested_component_value = 
# 283 "menhir_parser.mly"
                             ( Component_value.Bracket_block b )
# 1090 "menhir_parser.ml"
             in
            _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv378)) : 'freshtv380)) : 'freshtv382)) : 'freshtv384)) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv387 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv393 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv391 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc_in_nested_prelude)), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_in_nested_prelude_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 1110 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_in_nested_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv392)) : 'freshtv394)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv403 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv401 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_nested_paren_block = 
# 258 "menhir_parser.mly"
                                                                                 ( xs )
# 1133 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv399) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv397) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
            let (_endpos_b_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_nested_paren_block) : 'tv_nested_paren_block) = _v in
            let (_startpos_b_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos_b_ in
            let _endpos = _endpos_b_ in
            let _v : 'tv_common_nested_component_value = 
# 282 "menhir_parser.mly"
                           ( Component_value.Paren_block b )
# 1158 "menhir_parser.ml"
             in
            _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv396)) : 'freshtv398)) : 'freshtv400)) : 'freshtv402)) : 'freshtv404)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv405 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv427 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_hd_, _menhir_s, (hd : 'tv_starting_component_value_with_loc_in_prelude), _startpos_hd_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
        let _startpos = _startpos_hd_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nested_prelude = 
# 245 "menhir_parser.mly"
                                                                                                             ( hd ::  xs )
# 1179 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv421) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_nested_prelude) : 'tv_nested_prelude) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _v : 'tv_nested_prelude_with_loc = let _endpos = _endpos_xs_ in
        let _startpos = _startpos_xs_ in
        
# 241 "menhir_parser.mly"
                      ( (xs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1205 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv417 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv413 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMPERSAND ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AT_RULE _v ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AT_RULE_WITHOUT_BODY _v ->
                _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | COLON ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DELIM _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DIMENSION _v ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DOT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT_DIMENSION _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | HASH _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_BRACKET ->
                _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_PAREN ->
                _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NESTED_AT_RULE _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NUMBER _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPERATOR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RIGHT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv411 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_menhir_s : _menhir_state) = MenhirState105 in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv409 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__3_ : Lexing.position) = _endpos in
                let (_ : _menhir_state) = _menhir_s in
                ((let ((_menhir_stack, _menhir_s, (xs : 'tv_nested_prelude_with_loc), _startpos_xs_), _endpos__2_) = _menhir_stack in
                let _startpos = _startpos_xs_ in
                let _endpos = _endpos__3_ in
                let _v : 'tv_nested_style_rule = let _endpos = _endpos__3_ in
                let _startpos = _startpos_xs_ in
                
# 226 "menhir_parser.mly"
                                                          (
      { Style_rule.prelude = xs;
        block = [], Location.none;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 1279 "menhir_parser.ml"
                 in
                _menhir_goto_nested_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv410)) : 'freshtv412)
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNICODE_RANGE _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHITESPACE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv414)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv415 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)) : 'freshtv420)) : 'freshtv422)) : 'freshtv424)) : 'freshtv426)) : 'freshtv428)) : 'freshtv430)
    | _ ->
        _menhir_fail ()

and _menhir_reduce72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_in_nested_prelude_with_loc_ = 
# 212 "<standard.mly>"
    ( [] )
# 1309 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_in_nested_prelude_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_component_value_in_nested_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value_in_nested_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState70 | MenhirState73 | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv367) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv365) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_component_value_in_nested_prelude) : 'tv_component_value_in_nested_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_in_nested_prelude_with_loc = let _endpos = _endpos_c_ in
        let _startpos = _startpos_c_ in
        
# 299 "menhir_parser.mly"
                                          ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1336 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv363) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_component_value_in_nested_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv361 * Lexing.position * _menhir_state * 'tv_component_value_in_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SELECTOR_FUNCTION _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RIGHT_PAREN ->
            _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState79
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv362)) : 'freshtv364)) : 'freshtv366)) : 'freshtv368)
    | MenhirState99 | MenhirState69 | MenhirState71 | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_component_value_in_nested_prelude) : 'tv_component_value_in_nested_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_with_loc_in_nested_prelude = let _endpos = _endpos_c_ in
        let _startpos = _startpos_c_ in
        
# 249 "menhir_parser.mly"
                                          ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1409 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv371) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_component_value_with_loc_in_nested_prelude) = _v in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SELECTOR_FUNCTION _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACE | RIGHT_BRACKET | RIGHT_PAREN ->
            _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv370)) : 'freshtv372)) : 'freshtv374)) : 'freshtv376)
    | _ ->
        _menhir_fail ()

and _menhir_goto_style_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_style_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv359) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_style_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv357) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : 'tv_style_rule) : 'tv_style_rule) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_rule = 
# 62 "menhir_parser.mly"
                   ( Rule.Style_rule r )
# 1484 "menhir_parser.ml"
     in
    _menhir_goto_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv358)) : 'freshtv360)

and _menhir_goto_at_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_at_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState0 | MenhirState132 | MenhirState68 | MenhirState105 | MenhirState115 | MenhirState116 | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_at_rule) : 'tv_at_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declaration_or_at_rule = 
# 142 "menhir_parser.mly"
                ( Declaration_list.At_rule r )
# 1509 "menhir_parser.ml"
         in
        _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv350)) : 'freshtv352)
    | MenhirState142 | MenhirState60 | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv355) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_at_rule) : 'tv_at_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_rule = 
# 61 "menhir_parser.mly"
                ( Rule.At_rule r )
# 1530 "menhir_parser.ml"
         in
        _menhir_goto_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv354)) : 'freshtv356)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_component_value_with_loc_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv281 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1545 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv277 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1555 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv275 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1563 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 35 "menhir_parser.mly"
       (string)
# 1569 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value = 
# 182 "menhir_parser.mly"
                                                                   (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_))
    )
# 1579 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv276)) : 'freshtv278)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv279 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1589 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)) : 'freshtv282)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv285 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv283 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc), _startpos_x_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 1604 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv284)) : 'freshtv286)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv299 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv297 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_bracket_block = 
# 161 "menhir_parser.mly"
                                                                   ( xs )
# 1627 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv295) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((match _menhir_s with
            | MenhirState95 | MenhirState43 | MenhirState9 | MenhirState14 | MenhirState18 | MenhirState20 | MenhirState31 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bracket_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_bracket_block) : 'tv_bracket_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value = 
# 169 "menhir_parser.mly"
                      ( Component_value.Bracket_block b )
# 1654 "menhir_parser.ml"
                 in
                _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv288)) : 'freshtv290)
            | MenhirState142 | MenhirState7 | MenhirState60 | MenhirState130 | MenhirState66 | MenhirState61 | MenhirState8 | MenhirState54 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bracket_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_bracket_block) : 'tv_bracket_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value_in_prelude = 
# 214 "menhir_parser.mly"
                      ( Component_value.Bracket_block b )
# 1675 "menhir_parser.ml"
                 in
                _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv292)) : 'freshtv294)
            | _ ->
                _menhir_fail ()) : 'freshtv296)) : 'freshtv298)) : 'freshtv300)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv301 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv302)) : 'freshtv304)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 1692 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv307 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 1702 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv305 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 1710 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 36 "menhir_parser.mly"
       (string)
# 1716 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value = 
# 178 "menhir_parser.mly"
                                                                            (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_))
    )
# 1726 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv306)) : 'freshtv308)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv309 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 1736 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)) : 'freshtv312)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv329 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv325 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv323 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_paren_block = 
# 157 "menhir_parser.mly"
                                                               ( xs )
# 1760 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv321) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((match _menhir_s with
            | MenhirState95 | MenhirState43 | MenhirState9 | MenhirState14 | MenhirState18 | MenhirState31 | MenhirState20 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv315) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_paren_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv313) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_paren_block) : 'tv_paren_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value = 
# 168 "menhir_parser.mly"
                    ( Component_value.Paren_block b )
# 1787 "menhir_parser.ml"
                 in
                _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv314)) : 'freshtv316)
            | MenhirState142 | MenhirState7 | MenhirState60 | MenhirState130 | MenhirState66 | MenhirState61 | MenhirState54 | MenhirState8 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv319) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_paren_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv317) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_paren_block) : 'tv_paren_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value_in_prelude = 
# 213 "menhir_parser.mly"
                    ( Component_value.Paren_block b )
# 1808 "menhir_parser.ml"
                 in
                _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv318)) : 'freshtv320)
            | _ ->
                _menhir_fail ()) : 'freshtv322)) : 'freshtv324)) : 'freshtv326)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv327 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)) : 'freshtv330)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv337 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1825 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv333 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1835 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv331 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1843 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 35 "menhir_parser.mly"
       (string)
# 1849 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value_in_prelude = 
# 219 "menhir_parser.mly"
                                                                   (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_)) )
# 1858 "menhir_parser.ml"
             in
            _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv332)) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv335 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1868 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv347 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1877 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IMPORTANT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv339) = Obj.magic _menhir_stack in
            let (_endpos__1_ : Lexing.position) = _endpos in
            let (_startpos__1_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos__1_ in
            let _endpos = _endpos__1_ in
            let _v : 'tv_boption_IMPORTANT_ = 
# 136 "<standard.mly>"
    ( true )
# 1897 "menhir_parser.ml"
             in
            _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv340)) : 'freshtv342)
        | EOF | RIGHT_BRACE | SEMI_COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
            ((let (_, _startpos) = Obj.magic _menhir_stack in
            let _endpos = _startpos in
            let _v : 'tv_boption_IMPORTANT_ = 
# 134 "<standard.mly>"
    ( false )
# 1908 "menhir_parser.ml"
             in
            _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv344)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv345 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1918 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)) : 'freshtv348)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_WHITESPACE_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_WHITESPACE_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState95 | MenhirState43 | MenhirState9 | MenhirState14 | MenhirState18 | MenhirState31 | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_component_value = 
# 176 "menhir_parser.mly"
                              ( Component_value.Delim ":" )
# 1951 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv254)) : 'freshtv256)
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv259 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_component_value = 
# 177 "menhir_parser.mly"
                            ( Component_value.Delim "." )
# 1970 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv258)) : 'freshtv260)
        | HASH _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv263 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 37 "menhir_parser.mly"
       (string)
# 1980 "menhir_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv261 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos_h_ : Lexing.position) = _endpos in
            let ((h : (
# 37 "menhir_parser.mly"
       (string)
# 1990 "menhir_parser.ml"
            )) : (
# 37 "menhir_parser.mly"
       (string)
# 1994 "menhir_parser.ml"
            )) = _v in
            let (_startpos_h_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_h_ in
            let _v : 'tv_component_value = 
# 186 "menhir_parser.mly"
                                 ( Component_value.Hash h )
# 2003 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv262)) : 'freshtv264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv265 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)) : 'freshtv268)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv273 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 2018 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv269 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 2028 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DELIM _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DIMENSION _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT_DIMENSION _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FUNCTION _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_BRACKET ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_PAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NUMBER _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPERATOR _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SELECTOR_FUNCTION _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNICODE_RANGE _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | URI _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHITESPACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | COLON | DOT | HASH _ ->
                _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | EOF | IMPORTANT | RIGHT_BRACE | SEMI_COLON ->
                _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv271 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 2079 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)) : 'freshtv274)
    | _ ->
        _menhir_fail ()

and _menhir_goto_component_value : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv251) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_component_value) : 'tv_component_value) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_component_value_with_loc = let _endpos = _endpos_c_ in
    let _startpos = _startpos_c_ in
    
# 165 "menhir_parser.mly"
                        ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2107 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv247) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv245 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | EOF | IMPORTANT | RIGHT_BRACE | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31) : 'freshtv246)) : 'freshtv248)) : 'freshtv250)) : 'freshtv252)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce76 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_list_component_value_with_loc_in_nested_prelude_ = 
# 212 "<standard.mly>"
    ( [] )
# 2169 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_in_nested_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run70 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 36 "menhir_parser.mly"
       (string)
# 2176 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHT_PAREN ->
        _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 2227 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv243) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 27 "menhir_parser.mly"
       (string)
# 2238 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 2242 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value_in_nested_prelude = 
# 304 "menhir_parser.mly"
              ( Component_value.Ident i )
# 2250 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv244)

and _menhir_run73 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 2257 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHT_PAREN ->
        _menhir_reduce72 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_goto_common_nested_component_value : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_common_nested_component_value -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState99 | MenhirState69 | MenhirState70 | MenhirState71 | MenhirState86 | MenhirState73 | MenhirState79 ->
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
        let _v : 'tv_component_value_in_nested_prelude = 
# 303 "menhir_parser.mly"
                                      ( c )
# 2326 "menhir_parser.ml"
         in
        _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv226)) : 'freshtv228)
    | MenhirState0 | MenhirState132 | MenhirState68 | MenhirState105 | MenhirState115 | MenhirState116 | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv241) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_nested_component_value) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_nested_component_value) : 'tv_common_nested_component_value) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_starting_component_value_in_nested_prelude = 
# 294 "menhir_parser.mly"
                                      ( c )
# 2347 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_starting_component_value_in_nested_prelude) : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_starting_component_value_with_loc_in_prelude = let _endpos = _endpos_c_ in
        let _startpos = _startpos_c_ in
        
# 253 "menhir_parser.mly"
                                                   ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2374 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_with_loc_in_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | SELECTOR_FUNCTION _v ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState99 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACE ->
            _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState99
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv230)) : 'freshtv232)) : 'freshtv234)) : 'freshtv236)) : 'freshtv238)) : 'freshtv240)) : 'freshtv242)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_component_value_with_loc_in_prelude_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_in_prelude_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 2440 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_prelude_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv173 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 2450 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_prelude_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv171 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 2458 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_prelude_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 36 "menhir_parser.mly"
       (string)
# 2464 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_prelude_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value_in_prelude = 
# 216 "menhir_parser.mly"
                                                                                       (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_)) )
# 2473 "menhir_parser.ml"
             in
            _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv172)) : 'freshtv174)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv175 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 2483 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_prelude_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_prelude_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_prelude_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc_in_prelude), _startpos_x_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_prelude_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_in_prelude_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 2498 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_in_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv180)) : 'freshtv182)
    | MenhirState142 | MenhirState7 | MenhirState60 | MenhirState130 | MenhirState66 | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223 * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_prelude_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221 * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_prelude_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_xs_, _menhir_s, (xs : 'tv_list_component_value_with_loc_in_prelude_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_xs_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_prelude = 
# 109 "menhir_parser.mly"
                                                 ( xs )
# 2512 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_prelude) : 'tv_prelude) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _v : 'tv_prelude_with_loc = let _endpos = _endpos_xs_ in
        let _startpos = _startpos_xs_ in
        
# 105 "menhir_parser.mly"
               ( (xs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2538 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        match _menhir_s with
        | MenhirState7 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv187 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2552 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv183 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2562 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AT_RULE _v ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DELIM _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DIMENSION _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DOT ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT_DIMENSION _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FUNCTION _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_BRACKET ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_PAREN ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NUMBER _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OPERATOR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | SELECTOR_FUNCTION _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UNICODE_RANGE _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHITESPACE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHT_BRACE ->
                    _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | LEFT_BRACE ->
                    _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState60
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60) : 'freshtv184)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv185 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2622 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)) : 'freshtv188)
        | MenhirState61 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv195 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2631 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI_COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv191 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2641 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv189 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2649 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__3_ : Lexing.position) = _endpos in
                ((let ((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 33 "menhir_parser.mly"
       (string)
# 2655 "menhir_parser.ml"
                )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_) = _menhir_stack in
                let _startpos = _startpos_name_ in
                let _endpos = _endpos__3_ in
                let _v : 'tv_at_rule = let _endpos = _endpos__3_ in
                let _startpos = _startpos_name_ in
                
# 66 "menhir_parser.mly"
                                                                   (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Empty;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 2670 "menhir_parser.ml"
                 in
                _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv190)) : 'freshtv192)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv193 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2680 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)) : 'freshtv196)
        | MenhirState66 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv201 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2689 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv197 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2699 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AMPERSAND ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE _v ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DELIM _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DIMENSION _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DOT ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT_DIMENSION _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_BRACKET ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_PAREN ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NUMBER _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OPERATOR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UNICODE_RANGE _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHITESPACE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68) : 'freshtv198)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv199 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2753 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
        | MenhirState142 | MenhirState60 | MenhirState130 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv211 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AMPERSAND ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE _v ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DELIM _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DIMENSION _v ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DOT ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT_DIMENSION _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_BRACKET ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_PAREN ->
                    _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NUMBER _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OPERATOR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHT_BRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv205 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_menhir_s : _menhir_state) = MenhirState132 in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv203 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos__3_ : Lexing.position) = _endpos in
                    let (_ : _menhir_state) = _menhir_s in
                    ((let ((_menhir_stack, _menhir_s, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__2_) = _menhir_stack in
                    let _startpos = _startpos_xs_ in
                    let _endpos = _endpos__3_ in
                    let _v : 'tv_style_rule = let _endpos = _endpos__3_ in
                    let _startpos = _startpos_xs_ in
                    
# 90 "menhir_parser.mly"
                                                   (
      { Style_rule.prelude = xs;
        block = [], Location.none;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 2824 "menhir_parser.ml"
                     in
                    _menhir_goto_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv204)) : 'freshtv206)
                | STRING _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UNICODE_RANGE _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHITESPACE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv208)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv209 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)) : 'freshtv212)
        | _ ->
            _menhir_fail ()) : 'freshtv214)) : 'freshtv216)) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_rule_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_rule_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rule_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
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
# 2871 "menhir_parser.ml"
         in
        _menhir_goto_list_rule_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv140)) : 'freshtv142)
    | MenhirState142 | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rule_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167) = Obj.magic _menhir_stack in
        let (_endpos_rs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((rs : 'tv_list_rule_) : 'tv_list_rule_) = _v in
        let (_startpos_rs_ : Lexing.position) = _startpos in
        ((let _v : 'tv_stylesheet_without_eof = let _endpos = _endpos_rs_ in
        let _startpos = _startpos_rs_ in
        
# 53 "menhir_parser.mly"
                  ( (rs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2892 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_stylesheet_without_eof) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState60 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv149 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2905 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv145 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2915 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv143 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2923 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                let (_endpos__5_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 32 "menhir_parser.mly"
       (string)
# 2929 "menhir_parser.ml"
                )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__3_), _, (s : 'tv_stylesheet_without_eof)) = _menhir_stack in
                let _startpos = _startpos_name_ in
                let _endpos = _endpos__5_ in
                let _v : 'tv_at_rule = let _endpos = _endpos__5_ in
                let _startpos = _startpos_name_ in
                
# 73 "menhir_parser.mly"
                                                                                                      (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Stylesheet s;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 2944 "menhir_parser.ml"
                 in
                _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv144)) : 'freshtv146)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv147 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2954 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)) : 'freshtv150)
        | MenhirState142 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EOF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv157 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (s : 'tv_stylesheet_without_eof)) = _menhir_stack in
                let _v : (
# 43 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2973 "menhir_parser.ml"
                ) = 
# 49 "menhir_parser.mly"
                                  ( s )
# 2977 "menhir_parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv155) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 43 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2985 "menhir_parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv153) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 43 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2993 "menhir_parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv151) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : (
# 43 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 3001 "menhir_parser.ml"
                )) : (
# 43 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 3005 "menhir_parser.ml"
                )) = _v in
                (Obj.magic _1 : 'freshtv152)) : 'freshtv154)) : 'freshtv156)) : 'freshtv158)) : 'freshtv160)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)) : 'freshtv164)
        | _ ->
            _menhir_fail ()) : 'freshtv166)) : 'freshtv168)) : 'freshtv170)
    | _ ->
        _menhir_fail ()

and _menhir_goto_component_value_in_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value_in_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_in_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_component_value_in_prelude) : 'tv_component_value_in_prelude) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_component_value_with_loc_in_prelude = let _endpos = _endpos_c_ in
    let _startpos = _startpos_c_ in
    
# 195 "menhir_parser.mly"
                                   ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 3041 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv133) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc_in_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv131 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE | RIGHT_PAREN | SEMI_COLON ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv132)) : 'freshtv134)) : 'freshtv136)) : 'freshtv138)

and _menhir_reduce74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_with_loc_ = 
# 212 "<standard.mly>"
    ( [] )
# 3101 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : 'tv_option_WHITESPACE_ = 
# 115 "<standard.mly>"
    ( None )
# 3111 "menhir_parser.ml"
     in
    _menhir_goto_option_WHITESPACE_ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
    let (_endpos_x_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos_x_ : Lexing.position) = _startpos in
    ((let x = () in
    let _startpos = _startpos_x_ in
    let _v : 'tv_option_WHITESPACE_ = 
# 117 "<standard.mly>"
    ( Some x )
# 3128 "menhir_parser.ml"
     in
    _menhir_goto_option_WHITESPACE_ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv130)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 29 "menhir_parser.mly"
       (string)
# 3135 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv127) = Obj.magic _menhir_stack in
    let (_endpos_u_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((u : (
# 29 "menhir_parser.mly"
       (string)
# 3146 "menhir_parser.ml"
    )) : (
# 29 "menhir_parser.mly"
       (string)
# 3150 "menhir_parser.ml"
    )) = _v in
    let (_startpos_u_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_u_ in
    let _endpos = _endpos_u_ in
    let _v : 'tv_component_value = 
# 173 "menhir_parser.mly"
            ( Component_value.Uri u )
# 3158 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv128)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 39 "menhir_parser.mly"
       (string)
# 3165 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : (
# 39 "menhir_parser.mly"
       (string)
# 3176 "menhir_parser.ml"
    )) : (
# 39 "menhir_parser.mly"
       (string)
# 3180 "menhir_parser.ml"
    )) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_component_value = 
# 188 "menhir_parser.mly"
                      ( Component_value.Unicode_range r )
# 3188 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv126)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 28 "menhir_parser.mly"
       (string)
# 3195 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_endpos_s_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 28 "menhir_parser.mly"
       (string)
# 3206 "menhir_parser.ml"
    )) : (
# 28 "menhir_parser.mly"
       (string)
# 3210 "menhir_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_component_value = 
# 172 "menhir_parser.mly"
               ( Component_value.String s )
# 3218 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv124)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 36 "menhir_parser.mly"
       (string)
# 3225 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | RIGHT_PAREN ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run15 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 30 "menhir_parser.mly"
       (string)
# 3272 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_endpos_o_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((o : (
# 30 "menhir_parser.mly"
       (string)
# 3283 "menhir_parser.ml"
    )) : (
# 30 "menhir_parser.mly"
       (string)
# 3287 "menhir_parser.ml"
    )) = _v in
    let (_startpos_o_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_o_ in
    let _endpos = _endpos_o_ in
    let _v : 'tv_component_value = 
# 174 "menhir_parser.mly"
                 ( Component_value.Operator o )
# 3295 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv122)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 38 "menhir_parser.mly"
       (string)
# 3302 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PERCENTAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * Lexing.position * _menhir_state * (
# 38 "menhir_parser.mly"
       (string)
# 3314 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * Lexing.position * _menhir_state * (
# 38 "menhir_parser.mly"
       (string)
# 3322 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 38 "menhir_parser.mly"
       (string)
# 3328 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_component_value = 
# 170 "menhir_parser.mly"
                           ( Component_value.Percentage n )
# 3335 "menhir_parser.ml"
         in
        _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv114)) : 'freshtv116)
    | COLON | DELIM _ | DIMENSION _ | DOT | EOF | FLOAT_DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT | LEFT_BRACKET | LEFT_PAREN | NUMBER _ | OPERATOR _ | RIGHT_BRACE | RIGHT_BRACKET | RIGHT_PAREN | SELECTOR_FUNCTION _ | SEMI_COLON | STRING _ | UNICODE_RANGE _ | URI _ | WHITESPACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * Lexing.position * _menhir_state * (
# 38 "menhir_parser.mly"
       (string)
# 3343 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 38 "menhir_parser.mly"
       (string)
# 3348 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_component_value = 
# 187 "menhir_parser.mly"
               ( Component_value.Number n )
# 3355 "menhir_parser.ml"
         in
        _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv118)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * Lexing.position * _menhir_state * (
# 38 "menhir_parser.mly"
       (string)
# 3365 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 3373 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 27 "menhir_parser.mly"
       (string)
# 3384 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 3388 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value = 
# 171 "menhir_parser.mly"
              ( Component_value.Ident i )
# 3396 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv112)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 3403 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | RIGHT_PAREN ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run21 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 40 "menhir_parser.mly"
       (string * string * Types.dimension)
# 3450 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv109) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 40 "menhir_parser.mly"
       (string * string * Types.dimension)
# 3461 "menhir_parser.ml"
    )) : (
# 40 "menhir_parser.mly"
       (string * string * Types.dimension)
# 3465 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 189 "menhir_parser.mly"
                        ( Component_value.Float_dimension d )
# 3473 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv110)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 41 "menhir_parser.mly"
       (string * string)
# 3480 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv107) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 41 "menhir_parser.mly"
       (string * string)
# 3491 "menhir_parser.ml"
    )) : (
# 41 "menhir_parser.mly"
       (string * string)
# 3495 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 190 "menhir_parser.mly"
                  ( Component_value.Dimension d )
# 3503 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv108)

and _menhir_run23 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 31 "menhir_parser.mly"
       (string)
# 3510 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv105) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 31 "menhir_parser.mly"
       (string)
# 3521 "menhir_parser.ml"
    )) : (
# 31 "menhir_parser.mly"
       (string)
# 3525 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 175 "menhir_parser.mly"
              ( Component_value.Delim d )
# 3533 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv106)

and _menhir_goto_common_component_values_in_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_common_component_values_in_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState142 | MenhirState7 | MenhirState60 | MenhirState130 | MenhirState66 | MenhirState61 | MenhirState8 | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_component_values_in_prelude) : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_in_prelude = 
# 215 "menhir_parser.mly"
                                           ( c )
# 3558 "menhir_parser.ml"
         in
        _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv98)) : 'freshtv100)
    | MenhirState0 | MenhirState132 | MenhirState68 | MenhirState105 | MenhirState115 | MenhirState116 | MenhirState109 | MenhirState99 | MenhirState69 | MenhirState70 | MenhirState71 | MenhirState86 | MenhirState73 | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_component_values_in_prelude) : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_common_nested_component_value = 
# 281 "menhir_parser.mly"
                                           ( c )
# 3579 "menhir_parser.ml"
         in
        _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv102)) : 'freshtv104)
    | _ ->
        _menhir_fail ()

and _menhir_run69 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHT_PAREN ->
        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run73 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHT_BRACKET ->
        _menhir_reduce76 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run93 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 3682 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93

and _menhir_run74 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_common_nested_component_value = 
# 284 "menhir_parser.mly"
              ( Component_value.Ampersand )
# 3711 "menhir_parser.ml"
     in
    _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv96)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv36)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 3762 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 3771 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * Lexing.position * _menhir_state * 'tv_component_value_in_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 3790 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 3804 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState69 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv67 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 3818 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 3827 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState61 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 3836 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv73 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 3845 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 3859 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 3873 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv85 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 3887 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 3901 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 3910 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv94)

and _menhir_reduce78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_with_loc_in_prelude_ = 
# 212 "<standard.mly>"
    ( [] )
# 3926 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_in_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_rule_ = 
# 212 "<standard.mly>"
    ( [] )
# 3937 "menhir_parser.ml"
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
# 270 "menhir_parser.mly"
               ( Component_value.Delim "*" )
# 3954 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv34)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 39 "menhir_parser.mly"
       (string)
# 3961 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv31) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : (
# 39 "menhir_parser.mly"
       (string)
# 3972 "menhir_parser.ml"
    )) : (
# 39 "menhir_parser.mly"
       (string)
# 3976 "menhir_parser.ml"
    )) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 275 "menhir_parser.mly"
                      ( Component_value.Unicode_range r )
# 3984 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv32)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 28 "menhir_parser.mly"
       (string)
# 3991 "menhir_parser.ml"
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
# 4002 "menhir_parser.ml"
    )) : (
# 28 "menhir_parser.mly"
       (string)
# 4006 "menhir_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 267 "menhir_parser.mly"
               ( Component_value.String s )
# 4014 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv30)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 36 "menhir_parser.mly"
       (string)
# 4021 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RIGHT_PAREN ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run4 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 30 "menhir_parser.mly"
       (string)
# 4070 "menhir_parser.ml"
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
# 4081 "menhir_parser.ml"
    )) : (
# 30 "menhir_parser.mly"
       (string)
# 4085 "menhir_parser.ml"
    )) = _v in
    let (_startpos_o_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_o_ in
    let _endpos = _endpos_o_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 268 "menhir_parser.mly"
                 ( Component_value.Operator o )
# 4093 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv28)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 38 "menhir_parser.mly"
       (string)
# 4100 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PERCENTAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * Lexing.position * _menhir_state * (
# 38 "menhir_parser.mly"
       (string)
# 4112 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * Lexing.position * _menhir_state * (
# 38 "menhir_parser.mly"
       (string)
# 4120 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 38 "menhir_parser.mly"
       (string)
# 4126 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_common_component_values_in_prelude = 
# 266 "menhir_parser.mly"
                           ( Component_value.Percentage n )
# 4133 "menhir_parser.ml"
         in
        _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv20)) : 'freshtv22)
    | AMPERSAND | COLON | DELIM _ | DIMENSION _ | DOT | FLOAT_DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | LEFT_BRACE | LEFT_BRACKET | LEFT_PAREN | NUMBER _ | OPERATOR _ | RIGHT_BRACKET | RIGHT_PAREN | SELECTOR_FUNCTION _ | SEMI_COLON | STRING _ | UNICODE_RANGE _ | WHITESPACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * Lexing.position * _menhir_state * (
# 38 "menhir_parser.mly"
       (string)
# 4141 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 38 "menhir_parser.mly"
       (string)
# 4146 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_common_component_values_in_prelude = 
# 274 "menhir_parser.mly"
               ( Component_value.Number n )
# 4153 "menhir_parser.ml"
         in
        _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * Lexing.position * _menhir_state * (
# 38 "menhir_parser.mly"
       (string)
# 4163 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 32 "menhir_parser.mly"
       (string)
# 4171 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState7 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run9 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | RIGHT_PAREN ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run18 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState18 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | RIGHT_BRACKET ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run41 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 4306 "menhir_parser.ml"
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
# 4317 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 4321 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value_in_prelude = 
# 222 "menhir_parser.mly"
              ( Component_value.Ident i )
# 4329 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv18)

and _menhir_run42 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 37 "menhir_parser.mly"
       (string)
# 4336 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_endpos_h_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((h : (
# 37 "menhir_parser.mly"
       (string)
# 4347 "menhir_parser.ml"
    )) : (
# 37 "menhir_parser.mly"
       (string)
# 4351 "menhir_parser.ml"
    )) = _v in
    let (_startpos_h_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_h_ in
    let _endpos = _endpos_h_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 273 "menhir_parser.mly"
             ( Component_value.Hash h )
# 4359 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv16)

and _menhir_run43 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 4366 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | RIGHT_PAREN ->
        _menhir_reduce74 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run46 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 40 "menhir_parser.mly"
       (string * string * Types.dimension)
# 4413 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 40 "menhir_parser.mly"
       (string * string * Types.dimension)
# 4424 "menhir_parser.ml"
    )) : (
# 40 "menhir_parser.mly"
       (string * string * Types.dimension)
# 4428 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 276 "menhir_parser.mly"
                        ( Component_value.Float_dimension d )
# 4436 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv14)

and _menhir_run47 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
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
# 272 "menhir_parser.mly"
        ( Component_value.Delim "." )
# 4453 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv12)

and _menhir_run48 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 41 "menhir_parser.mly"
       (string * string)
# 4460 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 41 "menhir_parser.mly"
       (string * string)
# 4471 "menhir_parser.ml"
    )) : (
# 41 "menhir_parser.mly"
       (string * string)
# 4475 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 277 "menhir_parser.mly"
                  ( Component_value.Dimension d )
# 4483 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv10)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 31 "menhir_parser.mly"
       (string)
# 4490 "menhir_parser.ml"
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
# 4501 "menhir_parser.ml"
    )) : (
# 31 "menhir_parser.mly"
       (string)
# 4505 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 269 "menhir_parser.mly"
              ( Component_value.Delim d )
# 4513 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv8)

and _menhir_run50 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
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
# 271 "menhir_parser.mly"
          ( Component_value.Delim ":" )
# 4530 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv6)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 33 "menhir_parser.mly"
       (string)
# 4537 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState61 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI_COLON ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run66 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "menhir_parser.mly"
       (string)
# 4586 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

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
# 44 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 4647 "menhir_parser.ml"
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
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE _v ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run93 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run69 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
# 43 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 4705 "menhir_parser.ml"
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
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SELECTOR_FUNCTION _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF ->
        _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | LEFT_BRACE ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState142
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv2))

# 270 "<standard.mly>"
  

# 4769 "menhir_parser.ml"
