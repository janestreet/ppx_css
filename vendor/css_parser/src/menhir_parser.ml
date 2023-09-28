
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
  | MenhirState127
  | MenhirState117
  | MenhirState115
  | MenhirState101
  | MenhirState100
  | MenhirState94
  | MenhirState90
  | MenhirState84
  | MenhirState80
  | MenhirState78
  | MenhirState71
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


# 143 "menhir_parser.ml"

let rec _menhir_goto_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv507) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declarations) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv505) = Obj.magic _menhir_stack in
    let (_endpos_ds_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((ds : 'tv_declarations) : 'tv_declarations) = _v in
    let (_startpos_ds_ : Lexing.position) = _startpos in
    ((let _v : 'tv_declarations_with_loc = let _endpos = _endpos_ds_ in
    let _startpos = _startpos_ds_ in
    
# 112 "menhir_parser.mly"
                      ( (ds, Lex_buffer.make_loc_and_fix ~loc_ghost:true _startpos _endpos) )
# 164 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv503) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declarations_with_loc) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv471 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv467 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv465 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, (xs : 'tv_nested_prelude_with_loc), _startpos_xs_), _endpos__2_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_xs_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_nested_style_rule = let _endpos = _endpos__4_ in
            let _startpos = _startpos_xs_ in
            
# 207 "menhir_parser.mly"
                                                                                      (
      { Style_rule.prelude = xs;
        block = ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 199 "menhir_parser.ml"
             in
            _menhir_goto_nested_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv466)) : 'freshtv468)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv469 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)) : 'freshtv472)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv479 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 214 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv475 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 224 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv473 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 232 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 34 "menhir_parser.mly"
       (string)
# 238 "menhir_parser.ml"
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
# 253 "menhir_parser.ml"
             in
            _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv474)) : 'freshtv476)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv477 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 263 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)) : 'freshtv480)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv487 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv483 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv481 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
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
# 294 "menhir_parser.ml"
             in
            _menhir_goto_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv482)) : 'freshtv484)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv485 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)) : 'freshtv488)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv501 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv497 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv495 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _v : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 319 "menhir_parser.ml"
            ) = 
# 56 "menhir_parser.mly"
                                  ( ds )
# 323 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv493) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 331 "menhir_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv491) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 339 "menhir_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv489) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 347 "menhir_parser.ml"
            )) : (
# 43 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 351 "menhir_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv490)) : 'freshtv492)) : 'freshtv494)) : 'freshtv496)) : 'freshtv498)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv499 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv500)) : 'freshtv502)
    | _ ->
        _menhir_fail ()) : 'freshtv504)) : 'freshtv506)) : 'freshtv508)

and _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv463 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
    | SEMI_COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState100 in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE_WITHOUT_BODY _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NESTED_AT_RULE _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv457 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_), _endpos__2_, _) = _menhir_stack in
            let _startpos = _startpos_ds_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 119 "menhir_parser.mly"
                                                                                                    ( List.rev ds)
# 460 "menhir_parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv458)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv460)
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | RIGHT_BRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv461 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_ds_ in
        let _v : 'tv_declarations = 
# 118 "menhir_parser.mly"
                                                                                        ( List.rev ds)
# 482 "menhir_parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv462)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100) : 'freshtv464)

and _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv455 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI_COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv449 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE_WITHOUT_BODY _v ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NESTED_AT_RULE _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv447 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_), _endpos__2_) = _menhir_stack in
            let _startpos = _startpos_ds_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 117 "menhir_parser.mly"
                                                                                            ( List.rev ds )
# 553 "menhir_parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv448)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94) : 'freshtv450)
    | EOF | RIGHT_BRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv451 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_ds_ in
        let _v : 'tv_declarations = 
# 116 "menhir_parser.mly"
                                                                                ( List.rev ds )
# 569 "menhir_parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv452)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv454)) : 'freshtv456)

and _menhir_goto_nested_style_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_nested_style_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_nested_style_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : 'tv_nested_style_rule) : 'tv_nested_style_rule) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_nested_rule = 
# 136 "menhir_parser.mly"
                          ( Declaration_list.Style_rule r )
# 599 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv441) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_nested_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((match _menhir_s with
    | MenhirState0 | MenhirState117 | MenhirState90 | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv425) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue = 
# 130 "menhir_parser.mly"
                    ( [ r ] )
# 626 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv426)) : 'freshtv428)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv431 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
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
# 648 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv430)) : 'freshtv432)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv435) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv433) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_nested_rule) : 'tv_nested_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declaration_or_at_rule = 
# 142 "menhir_parser.mly"
                    ( r )
# 669 "menhir_parser.ml"
         in
        _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv434)) : 'freshtv436)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv439 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv437 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
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
# 691 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv438)) : 'freshtv440)
    | _ ->
        _menhir_fail ()) : 'freshtv442)) : 'freshtv444)) : 'freshtv446)

and _menhir_goto_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv423 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AT_RULE _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState115 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | RIGHT_BRACE ->
        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | LEFT_BRACE ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState115
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv424)

and _menhir_goto_declaration_or_at_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declaration_or_at_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv409 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
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
# 772 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv408)) : 'freshtv410)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv413 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv411 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
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
# 794 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv412)) : 'freshtv414)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv417 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv415 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
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
# 816 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv416)) : 'freshtv418)
    | MenhirState0 | MenhirState117 | MenhirState62 | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv421) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv419) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_d_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue = 
# 123 "menhir_parser.mly"
                               ( [d] )
# 837 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv420)) : 'freshtv422)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_IMPORTANT_ : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_boption_IMPORTANT_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv405 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 849 "menhir_parser.ml"
    ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_boption_IMPORTANT_) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv403 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 858 "menhir_parser.ml"
    ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let ((i : 'tv_boption_IMPORTANT_) : 'tv_boption_IMPORTANT_) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let ((((_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 27 "menhir_parser.mly"
       (string)
# 866 "menhir_parser.ml"
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
# 881 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv401) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv399) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv397) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : 'tv_declaration) : 'tv_declaration) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_declaration_or_at_rule = 
# 140 "menhir_parser.mly"
                    ( Declaration_list.Declaration d )
# 906 "menhir_parser.ml"
     in
    _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv398)) : 'freshtv400)) : 'freshtv402)) : 'freshtv404)) : 'freshtv406)

and _menhir_goto_list_component_value_with_loc_in_nested_prelude_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_in_nested_prelude_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv355 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv351 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv349 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_nested_bracket_block = 
# 237 "menhir_parser.mly"
                                                                                     ( xs )
# 934 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv347) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv345) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv343) = Obj.magic _menhir_stack in
            let (_endpos_b_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_nested_bracket_block) : 'tv_nested_bracket_block) = _v in
            let (_startpos_b_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos_b_ in
            let _endpos = _endpos_b_ in
            let _v : 'tv_common_nested_component_value = 
# 261 "menhir_parser.mly"
                             ( Component_value.Bracket_block b )
# 959 "menhir_parser.ml"
             in
            _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv344)) : 'freshtv346)) : 'freshtv348)) : 'freshtv350)) : 'freshtv352)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv353 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)) : 'freshtv356)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv359 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv357 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc_in_nested_prelude)), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_in_nested_prelude_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 979 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_in_nested_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv358)) : 'freshtv360)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv373 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv369 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv367 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_nested_paren_block = 
# 233 "menhir_parser.mly"
                                                                                 ( xs )
# 1002 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv365) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv363) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_nested_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv361) = Obj.magic _menhir_stack in
            let (_endpos_b_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_nested_paren_block) : 'tv_nested_paren_block) = _v in
            let (_startpos_b_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos_b_ in
            let _endpos = _endpos_b_ in
            let _v : 'tv_common_nested_component_value = 
# 260 "menhir_parser.mly"
                           ( Component_value.Paren_block b )
# 1027 "menhir_parser.ml"
             in
            _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv362)) : 'freshtv364)) : 'freshtv366)) : 'freshtv368)) : 'freshtv370)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv371 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)) : 'freshtv374)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv395 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv393 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_in_nested_prelude_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_hd_, _menhir_s, (hd : 'tv_starting_component_value_with_loc_in_prelude), _startpos_hd_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_in_nested_prelude_)) = _menhir_stack in
        let _startpos = _startpos_hd_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nested_prelude = 
# 220 "menhir_parser.mly"
                                                                                                             ( hd ::  xs )
# 1048 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv391) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv389) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv387) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_nested_prelude) : 'tv_nested_prelude) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _v : 'tv_nested_prelude_with_loc = let _endpos = _endpos_xs_ in
        let _startpos = _startpos_xs_ in
        
# 216 "menhir_parser.mly"
                      ( (xs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1074 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nested_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AMPERSAND ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AT_RULE _v ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | AT_RULE_WITHOUT_BODY _v ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | HASH _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_BRACKET ->
                _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_PAREN ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NESTED_AT_RULE _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NUMBER _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPERATOR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RIGHT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv377 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_menhir_s : _menhir_state) = MenhirState90 in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv375 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__3_ : Lexing.position) = _endpos in
                let (_ : _menhir_state) = _menhir_s in
                ((let ((_menhir_stack, _menhir_s, (xs : 'tv_nested_prelude_with_loc), _startpos_xs_), _endpos__2_) = _menhir_stack in
                let _startpos = _startpos_xs_ in
                let _endpos = _endpos__3_ in
                let _v : 'tv_nested_style_rule = let _endpos = _endpos__3_ in
                let _startpos = _startpos_xs_ in
                
# 201 "menhir_parser.mly"
                                                          (
      { Style_rule.prelude = xs;
        block = [], Location.none;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 1150 "menhir_parser.ml"
                 in
                _menhir_goto_nested_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv376)) : 'freshtv378)
            | STRING _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNICODE_RANGE _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHITESPACE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState90 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv380)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv381 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv382)) : 'freshtv384)) : 'freshtv386)) : 'freshtv388)) : 'freshtv390)) : 'freshtv392)) : 'freshtv394)) : 'freshtv396)
    | _ ->
        _menhir_fail ()

and _menhir_goto_component_value_in_nested_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value_in_nested_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_in_nested_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv339) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_component_value_in_nested_prelude) : 'tv_component_value_in_nested_prelude) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos_c_ in
    let _v : 'tv_component_value_with_loc_in_nested_prelude = let _endpos = _endpos_c_ in
    let _startpos = _startpos_c_ in
    
# 224 "menhir_parser.mly"
                                          ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1193 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv337) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc_in_nested_prelude) = _v in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv335 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE | RIGHT_BRACKET | RIGHT_PAREN ->
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv336)) : 'freshtv338)) : 'freshtv340)) : 'freshtv342)

and _menhir_goto_style_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_style_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv333) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_style_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv331) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : 'tv_style_rule) : 'tv_style_rule) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_rule = 
# 61 "menhir_parser.mly"
                   ( Rule.Style_rule r )
# 1264 "menhir_parser.ml"
     in
    _menhir_goto_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv332)) : 'freshtv334)

and _menhir_goto_at_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_at_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState0 | MenhirState117 | MenhirState62 | MenhirState90 | MenhirState100 | MenhirState101 | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv323) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_at_rule) : 'tv_at_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declaration_or_at_rule = 
# 141 "menhir_parser.mly"
                ( Declaration_list.At_rule r )
# 1289 "menhir_parser.ml"
         in
        _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv324)) : 'freshtv326)
    | MenhirState127 | MenhirState48 | MenhirState115 ->
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
        let _v : 'tv_rule = 
# 60 "menhir_parser.mly"
                ( Rule.At_rule r )
# 1310 "menhir_parser.ml"
         in
        _menhir_goto_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv328)) : 'freshtv330)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_component_value_with_loc_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1325 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1335 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1343 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 35 "menhir_parser.mly"
       (string)
# 1349 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value = 
# 177 "menhir_parser.mly"
                                                                   (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_))
    )
# 1359 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv258)) : 'freshtv260)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1369 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv267 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc), _startpos_x_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 1384 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv266)) : 'freshtv268)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv285 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv281 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv279 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_bracket_block = 
# 160 "menhir_parser.mly"
                                                                   ( xs )
# 1407 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv277) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((match _menhir_s with
            | MenhirState80 | MenhirState39 | MenhirState8 | MenhirState16 | MenhirState18 | MenhirState29 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv271) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bracket_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv269) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_bracket_block) : 'tv_bracket_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value = 
# 168 "menhir_parser.mly"
                      ( Component_value.Bracket_block b )
# 1434 "menhir_parser.ml"
                 in
                _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv270)) : 'freshtv272)
            | MenhirState127 | MenhirState7 | MenhirState48 | MenhirState115 | MenhirState60 | MenhirState49 | MenhirState55 ->
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
                let _v : 'tv_component_value_in_prelude = 
# 195 "menhir_parser.mly"
                      ( Component_value.Bracket_block b )
# 1455 "menhir_parser.ml"
                 in
                _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv274)) : 'freshtv276)
            | _ ->
                _menhir_fail ()) : 'freshtv278)) : 'freshtv280)) : 'freshtv282)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv283 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)) : 'freshtv286)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
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
            let _v : 'tv_paren_block = 
# 156 "menhir_parser.mly"
                                                               ( xs )
# 1487 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv295) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((match _menhir_s with
            | MenhirState80 | MenhirState39 | MenhirState8 | MenhirState16 | MenhirState29 | MenhirState18 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_paren_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_paren_block) : 'tv_paren_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value = 
# 167 "menhir_parser.mly"
                    ( Component_value.Paren_block b )
# 1514 "menhir_parser.ml"
                 in
                _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv288)) : 'freshtv290)
            | MenhirState127 | MenhirState7 | MenhirState48 | MenhirState115 | MenhirState60 | MenhirState55 | MenhirState49 ->
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
                let _v : 'tv_component_value_in_prelude = 
# 194 "menhir_parser.mly"
                    ( Component_value.Paren_block b )
# 1535 "menhir_parser.ml"
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
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv311 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1552 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv307 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1562 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv305 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1570 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 35 "menhir_parser.mly"
       (string)
# 1576 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_common_component_values_in_prelude = 
# 248 "menhir_parser.mly"
                                                                   (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_)) )
# 1585 "menhir_parser.ml"
             in
            _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv306)) : 'freshtv308)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv309 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 1595 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)) : 'freshtv312)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv321 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1604 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IMPORTANT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv315) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv313) = Obj.magic _menhir_stack in
            let (_endpos__1_ : Lexing.position) = _endpos in
            let (_startpos__1_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos__1_ in
            let _endpos = _endpos__1_ in
            let _v : 'tv_boption_IMPORTANT_ = 
# 136 "<standard.mly>"
    ( true )
# 1624 "menhir_parser.ml"
             in
            _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv314)) : 'freshtv316)
        | EOF | RIGHT_BRACE | SEMI_COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv317) = Obj.magic _menhir_stack in
            ((let (_, _startpos) = Obj.magic _menhir_stack in
            let _endpos = _startpos in
            let _v : 'tv_boption_IMPORTANT_ = 
# 134 "<standard.mly>"
    ( false )
# 1635 "menhir_parser.ml"
             in
            _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv319 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1645 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_WHITESPACE_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_WHITESPACE_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState80 | MenhirState39 | MenhirState8 | MenhirState16 | MenhirState29 | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_component_value = 
# 175 "menhir_parser.mly"
                              ( Component_value.Delim ":" )
# 1678 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv236)) : 'freshtv238)
        | DOT ->
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
# 176 "menhir_parser.mly"
                            ( Component_value.Delim "." )
# 1697 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv240)) : 'freshtv242)
        | HASH _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv245 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 36 "menhir_parser.mly"
       (string)
# 1707 "menhir_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos_h_ : Lexing.position) = _endpos in
            let ((h : (
# 36 "menhir_parser.mly"
       (string)
# 1717 "menhir_parser.ml"
            )) : (
# 36 "menhir_parser.mly"
       (string)
# 1721 "menhir_parser.ml"
            )) = _v in
            let (_startpos_h_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_h_ in
            let _v : 'tv_component_value = 
# 181 "menhir_parser.mly"
                                 ( Component_value.Hash h )
# 1730 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv244)) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv247 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv255 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1745 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv251 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1755 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DELIM _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DIMENSION _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT_DIMENSION _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FUNCTION _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_BRACKET ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_PAREN ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NUMBER _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPERATOR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNICODE_RANGE _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | URI _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHITESPACE ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState80 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | COLON | DOT | HASH _ ->
                _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | EOF | IMPORTANT | RIGHT_BRACE | SEMI_COLON ->
                _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv252)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv253 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 1804 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)) : 'freshtv256)
    | _ ->
        _menhir_fail ()

and _menhir_goto_component_value : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv233) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv231) = Obj.magic _menhir_stack in
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
# 1832 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv229) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv227 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) = Obj.magic _menhir_stack in
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
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | EOF | IMPORTANT | RIGHT_BRACE | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv228)) : 'freshtv230)) : 'freshtv232)) : 'freshtv234)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_list_component_value_with_loc_in_nested_prelude_ = 
# 212 "<standard.mly>"
    ( [] )
# 1892 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_in_nested_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 1899 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv225) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 27 "menhir_parser.mly"
       (string)
# 1910 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 1914 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value_in_nested_prelude = 
# 278 "menhir_parser.mly"
              ( Component_value.Ident i )
# 1922 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv226)

and _menhir_goto_common_nested_component_value : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_common_nested_component_value -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState84 | MenhirState63 | MenhirState64 | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_nested_component_value) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_nested_component_value) : 'tv_common_nested_component_value) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_in_nested_prelude = 
# 277 "menhir_parser.mly"
                                      ( c )
# 1947 "menhir_parser.ml"
         in
        _menhir_goto_component_value_in_nested_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv208)) : 'freshtv210)
    | MenhirState0 | MenhirState117 | MenhirState62 | MenhirState90 | MenhirState100 | MenhirState101 | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv223) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_nested_component_value) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv221) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_nested_component_value) : 'tv_common_nested_component_value) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_starting_component_value_in_nested_prelude = 
# 272 "menhir_parser.mly"
                                      ( c )
# 1968 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_starting_component_value_in_nested_prelude) : 'tv_starting_component_value_in_nested_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_starting_component_value_with_loc_in_prelude = let _endpos = _endpos_c_ in
        let _startpos = _startpos_c_ in
        
# 228 "menhir_parser.mly"
                                                   ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1995 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_starting_component_value_with_loc_in_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AMPERSAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | COLON ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DELIM _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DIMENSION _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOT ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FLOAT_DIMENSION _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FUNCTION _v ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | HASH _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACKET ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_PAREN ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NUMBER _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPERATOR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | STRING _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNICODE_RANGE _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WHITESPACE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LEFT_BRACE ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv212)) : 'freshtv214)) : 'freshtv216)) : 'freshtv218)) : 'freshtv220)) : 'freshtv222)) : 'freshtv224)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_component_value_with_loc_in_prelude_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_in_prelude_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState127 | MenhirState7 | MenhirState48 | MenhirState115 | MenhirState60 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_component_value_with_loc_in_prelude_) : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_prelude = 
# 108 "menhir_parser.mly"
                                                 ( xs )
# 2071 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv197) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_prelude) : 'tv_prelude) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _v : 'tv_prelude_with_loc = let _endpos = _endpos_xs_ in
        let _startpos = _startpos_xs_ in
        
# 104 "menhir_parser.mly"
               ( (xs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2097 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        match _menhir_s with
        | MenhirState7 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv165 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2111 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv161 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2121 "menhir_parser.ml"
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
                    _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | LEFT_BRACE ->
                    _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState48
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv162)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv163 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2179 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)) : 'freshtv166)
        | MenhirState49 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv173 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2188 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI_COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv169 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2198 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv167 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2206 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__3_ : Lexing.position) = _endpos in
                ((let ((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 33 "menhir_parser.mly"
       (string)
# 2212 "menhir_parser.ml"
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
# 2227 "menhir_parser.ml"
                 in
                _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv168)) : 'freshtv170)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv171 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2237 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)) : 'freshtv174)
        | MenhirState60 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv179 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2246 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv175 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2256 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AMPERSAND ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
                | FUNCTION _v ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv176)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv177 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2312 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv178)) : 'freshtv180)
        | MenhirState127 | MenhirState48 | MenhirState115 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv185 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AMPERSAND ->
                    _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DELIM _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DIMENSION _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DOT ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT_DIMENSION _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FUNCTION _v ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_BRACKET ->
                    _menhir_run64 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_PAREN ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NUMBER _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OPERATOR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHT_BRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv183 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_menhir_s : _menhir_state) = MenhirState117 in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv181 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
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
# 2385 "menhir_parser.ml"
                     in
                    _menhir_goto_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv182)) : 'freshtv184)
                | STRING _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UNICODE_RANGE _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHITESPACE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv186)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv187 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)) : 'freshtv190)
        | _ ->
            _menhir_fail ()) : 'freshtv192)) : 'freshtv194)) : 'freshtv196)) : 'freshtv198)) : 'freshtv200)) : 'freshtv202)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv205 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv203 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
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
# 2426 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_in_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv204)) : 'freshtv206)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_rule_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_rule_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rule_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv129 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
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
# 2454 "menhir_parser.ml"
         in
        _menhir_goto_list_rule_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv130)) : 'freshtv132)
    | MenhirState127 | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rule_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
        let (_endpos_rs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((rs : 'tv_list_rule_) : 'tv_list_rule_) = _v in
        let (_startpos_rs_ : Lexing.position) = _startpos in
        ((let _v : 'tv_stylesheet_without_eof = let _endpos = _endpos_rs_ in
        let _startpos = _startpos_rs_ in
        
# 52 "menhir_parser.mly"
                  ( (rs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2475 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_stylesheet_without_eof) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState48 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv139 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2488 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv135 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2498 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv133 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2506 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                let (_endpos__5_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 32 "menhir_parser.mly"
       (string)
# 2512 "menhir_parser.ml"
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
# 2527 "menhir_parser.ml"
                 in
                _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv134)) : 'freshtv136)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv137 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2537 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)) : 'freshtv140)
        | MenhirState127 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv153 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EOF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv149 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv147 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (s : 'tv_stylesheet_without_eof)) = _menhir_stack in
                let _v : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2556 "menhir_parser.ml"
                ) = 
# 48 "menhir_parser.mly"
                                  ( s )
# 2560 "menhir_parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv145) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2568 "menhir_parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv143) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2576 "menhir_parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv141) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2584 "menhir_parser.ml"
                )) : (
# 42 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 2588 "menhir_parser.ml"
                )) = _v in
                (Obj.magic _1 : 'freshtv142)) : 'freshtv144)) : 'freshtv146)) : 'freshtv148)) : 'freshtv150)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv151 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)) : 'freshtv154)
        | _ ->
            _menhir_fail ()) : 'freshtv156)) : 'freshtv158)) : 'freshtv160)
    | _ ->
        _menhir_fail ()

and _menhir_goto_component_value_in_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value_in_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv127) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_in_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
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
# 2624 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc_in_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
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
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState55
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55) : 'freshtv122)) : 'freshtv124)) : 'freshtv126)) : 'freshtv128)

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_with_loc_ = 
# 212 "<standard.mly>"
    ( [] )
# 2682 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : 'tv_option_WHITESPACE_ = 
# 115 "<standard.mly>"
    ( None )
# 2692 "menhir_parser.ml"
     in
    _menhir_goto_option_WHITESPACE_ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run9 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
    let (_endpos_x_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos_x_ : Lexing.position) = _startpos in
    ((let x = () in
    let _startpos = _startpos_x_ in
    let _v : 'tv_option_WHITESPACE_ = 
# 117 "<standard.mly>"
    ( Some x )
# 2709 "menhir_parser.ml"
     in
    _menhir_goto_option_WHITESPACE_ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv120)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 29 "menhir_parser.mly"
       (string)
# 2716 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv117) = Obj.magic _menhir_stack in
    let (_endpos_u_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((u : (
# 29 "menhir_parser.mly"
       (string)
# 2727 "menhir_parser.ml"
    )) : (
# 29 "menhir_parser.mly"
       (string)
# 2731 "menhir_parser.ml"
    )) = _v in
    let (_startpos_u_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_u_ in
    let _endpos = _endpos_u_ in
    let _v : 'tv_component_value = 
# 172 "menhir_parser.mly"
            ( Component_value.Uri u )
# 2739 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv118)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 38 "menhir_parser.mly"
       (string)
# 2746 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv115) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : (
# 38 "menhir_parser.mly"
       (string)
# 2757 "menhir_parser.ml"
    )) : (
# 38 "menhir_parser.mly"
       (string)
# 2761 "menhir_parser.ml"
    )) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_component_value = 
# 183 "menhir_parser.mly"
                      ( Component_value.Unicode_range r )
# 2769 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv116)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 28 "menhir_parser.mly"
       (string)
# 2776 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
    let (_endpos_s_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 28 "menhir_parser.mly"
       (string)
# 2787 "menhir_parser.ml"
    )) : (
# 28 "menhir_parser.mly"
       (string)
# 2791 "menhir_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_component_value = 
# 171 "menhir_parser.mly"
               ( Component_value.String s )
# 2799 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv114)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 30 "menhir_parser.mly"
       (string)
# 2806 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
    let (_endpos_o_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((o : (
# 30 "menhir_parser.mly"
       (string)
# 2817 "menhir_parser.ml"
    )) : (
# 30 "menhir_parser.mly"
       (string)
# 2821 "menhir_parser.ml"
    )) = _v in
    let (_startpos_o_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_o_ in
    let _endpos = _endpos_o_ in
    let _v : 'tv_component_value = 
# 173 "menhir_parser.mly"
                 ( Component_value.Operator o )
# 2829 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv112)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 37 "menhir_parser.mly"
       (string)
# 2836 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PERCENTAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 2848 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 2856 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 37 "menhir_parser.mly"
       (string)
# 2862 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_component_value = 
# 169 "menhir_parser.mly"
                           ( Component_value.Percentage n )
# 2869 "menhir_parser.ml"
         in
        _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv104)) : 'freshtv106)
    | COLON | DELIM _ | DIMENSION _ | DOT | EOF | FLOAT_DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT | LEFT_BRACKET | LEFT_PAREN | NUMBER _ | OPERATOR _ | RIGHT_BRACE | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON | STRING _ | UNICODE_RANGE _ | URI _ | WHITESPACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 2877 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 37 "menhir_parser.mly"
       (string)
# 2882 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_component_value = 
# 182 "menhir_parser.mly"
               ( Component_value.Number n )
# 2889 "menhir_parser.ml"
         in
        _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv108)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 2899 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 2907 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 27 "menhir_parser.mly"
       (string)
# 2918 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 2922 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value = 
# 170 "menhir_parser.mly"
              ( Component_value.Ident i )
# 2930 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv102)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 2937 "menhir_parser.ml"
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
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | RIGHT_PAREN ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 2982 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 2993 "menhir_parser.ml"
    )) : (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 2997 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 184 "menhir_parser.mly"
                        ( Component_value.Float_dimension d )
# 3005 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv100)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 40 "menhir_parser.mly"
       (string * string)
# 3012 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv97) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 40 "menhir_parser.mly"
       (string * string)
# 3023 "menhir_parser.ml"
    )) : (
# 40 "menhir_parser.mly"
       (string * string)
# 3027 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 185 "menhir_parser.mly"
                  ( Component_value.Dimension d )
# 3035 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv98)

and _menhir_run21 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 31 "menhir_parser.mly"
       (string)
# 3042 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv95) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 31 "menhir_parser.mly"
       (string)
# 3053 "menhir_parser.ml"
    )) : (
# 31 "menhir_parser.mly"
       (string)
# 3057 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 174 "menhir_parser.mly"
              ( Component_value.Delim d )
# 3065 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv96)

and _menhir_goto_common_component_values_in_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_common_component_values_in_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState127 | MenhirState7 | MenhirState48 | MenhirState115 | MenhirState60 | MenhirState49 | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_common_component_values_in_prelude) : 'tv_common_component_values_in_prelude) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_component_value_in_prelude = 
# 196 "menhir_parser.mly"
                                           ( c )
# 3090 "menhir_parser.ml"
         in
        _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv88)) : 'freshtv90)
    | MenhirState0 | MenhirState117 | MenhirState62 | MenhirState90 | MenhirState100 | MenhirState101 | MenhirState94 | MenhirState84 | MenhirState63 | MenhirState64 | MenhirState71 ->
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
        let _v : 'tv_common_nested_component_value = 
# 259 "menhir_parser.mly"
                                           ( c )
# 3111 "menhir_parser.ml"
         in
        _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv92)) : 'freshtv94)
    | _ ->
        _menhir_fail ()

and _menhir_run63 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AMPERSAND ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState63
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
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run78 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 3210 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WHITESPACE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState78
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_run66 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv85) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_common_nested_component_value = 
# 262 "menhir_parser.mly"
              ( Component_value.Delim "&" )
# 3239 "menhir_parser.ml"
     in
    _menhir_goto_common_nested_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv86)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv36)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState100 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_does_not_need_semicolon_to_continue * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon_but_needs_semicolon_to_continue * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv47 * _menhir_state * 'tv_nested_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * Lexing.position * _menhir_state * 'tv_starting_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 3290 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * Lexing.position * _menhir_state * (
# 27 "menhir_parser.mly"
       (string)
# 3299 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_nested_prelude) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv61 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 3323 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 3332 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState55 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 3346 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv69 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 3355 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState39 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 3364 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv73 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * Lexing.position * _menhir_state * (
# 35 "menhir_parser.mly"
       (string)
# 3378 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState16 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 3397 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv84)

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_with_loc_in_prelude_ = 
# 212 "<standard.mly>"
    ( [] )
# 3413 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_in_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_rule_ = 
# 212 "<standard.mly>"
    ( [] )
# 3424 "menhir_parser.ml"
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
# 245 "menhir_parser.mly"
               ( Component_value.Delim "*" )
# 3441 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv34)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 38 "menhir_parser.mly"
       (string)
# 3448 "menhir_parser.ml"
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
# 3459 "menhir_parser.ml"
    )) : (
# 38 "menhir_parser.mly"
       (string)
# 3463 "menhir_parser.ml"
    )) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 253 "menhir_parser.mly"
                      ( Component_value.Unicode_range r )
# 3471 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv32)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 28 "menhir_parser.mly"
       (string)
# 3478 "menhir_parser.ml"
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
# 3489 "menhir_parser.ml"
    )) : (
# 28 "menhir_parser.mly"
       (string)
# 3493 "menhir_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 242 "menhir_parser.mly"
               ( Component_value.String s )
# 3501 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv30)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 30 "menhir_parser.mly"
       (string)
# 3508 "menhir_parser.ml"
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
# 3519 "menhir_parser.ml"
    )) : (
# 30 "menhir_parser.mly"
       (string)
# 3523 "menhir_parser.ml"
    )) = _v in
    let (_startpos_o_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_o_ in
    let _endpos = _endpos_o_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 243 "menhir_parser.mly"
                 ( Component_value.Operator o )
# 3531 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv28)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 37 "menhir_parser.mly"
       (string)
# 3538 "menhir_parser.ml"
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
# 3550 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3558 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 37 "menhir_parser.mly"
       (string)
# 3564 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_common_component_values_in_prelude = 
# 241 "menhir_parser.mly"
                           ( Component_value.Percentage n )
# 3571 "menhir_parser.ml"
         in
        _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv20)) : 'freshtv22)
    | AMPERSAND | COLON | DELIM _ | DIMENSION _ | DOT | FLOAT_DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | LEFT_BRACE | LEFT_BRACKET | LEFT_PAREN | NUMBER _ | OPERATOR _ | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON | STRING _ | UNICODE_RANGE _ | WHITESPACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3579 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 37 "menhir_parser.mly"
       (string)
# 3584 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_common_component_values_in_prelude = 
# 252 "menhir_parser.mly"
               ( Component_value.Number n )
# 3591 "menhir_parser.ml"
         in
        _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * Lexing.position * _menhir_state * (
# 37 "menhir_parser.mly"
       (string)
# 3601 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 32 "menhir_parser.mly"
       (string)
# 3609 "menhir_parser.ml"
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
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState7
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
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | RIGHT_PAREN ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState8
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
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | RIGHT_BRACKET ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run37 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 3738 "menhir_parser.ml"
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
# 3749 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 3753 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value_in_prelude = 
# 197 "menhir_parser.mly"
              ( Component_value.Ident i )
# 3761 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv18)

and _menhir_run38 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 36 "menhir_parser.mly"
       (string)
# 3768 "menhir_parser.ml"
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
# 3779 "menhir_parser.ml"
    )) : (
# 36 "menhir_parser.mly"
       (string)
# 3783 "menhir_parser.ml"
    )) = _v in
    let (_startpos_h_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_h_ in
    let _endpos = _endpos_h_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 251 "menhir_parser.mly"
             ( Component_value.Hash h )
# 3791 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv16)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 3798 "menhir_parser.ml"
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
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | RIGHT_PAREN ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run42 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 3843 "menhir_parser.ml"
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
# 3854 "menhir_parser.ml"
    )) : (
# 39 "menhir_parser.mly"
       (string * string * Types.dimension)
# 3858 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 254 "menhir_parser.mly"
                        ( Component_value.Float_dimension d )
# 3866 "menhir_parser.ml"
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
# 247 "menhir_parser.mly"
        ( Component_value.Delim "." )
# 3883 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv12)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 40 "menhir_parser.mly"
       (string * string)
# 3890 "menhir_parser.ml"
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
# 3901 "menhir_parser.ml"
    )) : (
# 40 "menhir_parser.mly"
       (string * string)
# 3905 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 255 "menhir_parser.mly"
                  ( Component_value.Dimension d )
# 3913 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv10)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 31 "menhir_parser.mly"
       (string)
# 3920 "menhir_parser.ml"
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
# 3931 "menhir_parser.ml"
    )) : (
# 31 "menhir_parser.mly"
       (string)
# 3935 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_common_component_values_in_prelude = 
# 244 "menhir_parser.mly"
              ( Component_value.Delim d )
# 3943 "menhir_parser.ml"
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
# 246 "menhir_parser.mly"
          ( Component_value.Delim ":" )
# 3960 "menhir_parser.ml"
     in
    _menhir_goto_common_component_values_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv6)

and _menhir_run49 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 33 "menhir_parser.mly"
       (string)
# 3967 "menhir_parser.ml"
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
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run60 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "menhir_parser.mly"
       (string)
# 4014 "menhir_parser.ml"
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
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState60
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
# 4073 "menhir_parser.ml"
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
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run78 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
# 4133 "menhir_parser.ml"
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
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF ->
        _menhir_reduce73 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LEFT_BRACE ->
        _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv2))

# 270 "<standard.mly>"
  

# 4195 "menhir_parser.ml"
