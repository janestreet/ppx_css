
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHITESPACE
    | URI of (
# 28 "menhir_parser.mly"
       (string)
# 12 "menhir_parser.ml"
  )
    | UNICODE_RANGE of (
# 37 "menhir_parser.mly"
       (string)
# 17 "menhir_parser.ml"
  )
    | STRING of (
# 27 "menhir_parser.mly"
       (string)
# 22 "menhir_parser.ml"
  )
    | SEMI_COLON
    | RIGHT_PAREN
    | RIGHT_BRACKET
    | RIGHT_BRACE
    | PERCENTAGE
    | OPERATOR of (
# 29 "menhir_parser.mly"
       (string)
# 32 "menhir_parser.ml"
  )
    | NUMBER of (
# 36 "menhir_parser.mly"
       (string)
# 37 "menhir_parser.ml"
  )
    | NESTED_AT_RULE of (
# 31 "menhir_parser.mly"
       (string)
# 42 "menhir_parser.ml"
  )
    | LEFT_PAREN
    | LEFT_BRACKET
    | LEFT_BRACE
    | IMPORTANT
    | IDENT of (
# 26 "menhir_parser.mly"
       (string)
# 51 "menhir_parser.ml"
  )
    | HASH of (
# 35 "menhir_parser.mly"
       (string)
# 56 "menhir_parser.ml"
  )
    | FUNCTION of (
# 34 "menhir_parser.mly"
       (string)
# 61 "menhir_parser.ml"
  )
    | FLOAT_DIMENSION of (
# 38 "menhir_parser.mly"
       (string * string * Types.dimension)
# 66 "menhir_parser.ml"
  )
    | EOF
    | DOT
    | DIMENSION of (
# 39 "menhir_parser.mly"
       (string * string)
# 73 "menhir_parser.ml"
  )
    | DELIM of (
# 30 "menhir_parser.mly"
       (string)
# 78 "menhir_parser.ml"
  )
    | COLON
    | AT_RULE_WITHOUT_BODY of (
# 32 "menhir_parser.mly"
       (string)
# 84 "menhir_parser.ml"
  )
    | AT_RULE of (
# 33 "menhir_parser.mly"
       (string)
# 89 "menhir_parser.ml"
  )
  
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
  | MenhirState93
  | MenhirState83
  | MenhirState81
  | MenhirState70
  | MenhirState65
  | MenhirState63
  | MenhirState62
  | MenhirState60
  | MenhirState56
  | MenhirState50
  | MenhirState49
  | MenhirState40
  | MenhirState30
  | MenhirState19
  | MenhirState17
  | MenhirState9
  | MenhirState1
  | MenhirState0

# 1 "menhir_parser.mly"
  

(* Workaround for this dune bug: https://github.com/ocaml/dune/issues/2450 *)
module Css = struct end

open Types


# 135 "menhir_parser.ml"

let rec _menhir_goto_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv357) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declarations) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv355) = Obj.magic _menhir_stack in
    let (_endpos_ds_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((ds : 'tv_declarations) : 'tv_declarations) = _v in
    let (_startpos_ds_ : Lexing.position) = _startpos in
    ((let _v : 'tv_declarations_with_loc = let _endpos = _endpos_ds_ in
    let _startpos = _startpos_ds_ in
    
# 111 "menhir_parser.mly"
                      ( (ds, Lex_buffer.make_loc_and_fix ~loc_ghost:true _startpos _endpos) )
# 156 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv353) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declarations_with_loc) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv329 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 169 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv325 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 179 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv323 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 187 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            ((let ((((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 33 "menhir_parser.mly"
       (string)
# 193 "menhir_parser.ml"
            )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__3_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_name_ in
            let _endpos = _endpos__5_ in
            let _v : 'tv_at_rule = let _endpos = _endpos__5_ in
            let _startpos = _startpos_name_ in
            
# 78 "menhir_parser.mly"
                                                                                               (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Declaration_list ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 208 "menhir_parser.ml"
             in
            _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv324)) : 'freshtv326)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv327 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 218 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)) : 'freshtv330)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv337 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv333 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv331 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__2_), _, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _startpos = _startpos_xs_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_style_rule = let _endpos = _endpos__4_ in
            let _startpos = _startpos_xs_ in
            
# 94 "menhir_parser.mly"
                                                                               (
      { Style_rule.prelude = xs;
        block = ds;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 249 "menhir_parser.ml"
             in
            _menhir_goto_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv332)) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv335 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)) : 'freshtv338)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv347 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv345 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (ds : 'tv_declarations_with_loc)) = _menhir_stack in
            let _v : (
# 42 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 274 "menhir_parser.ml"
            ) = 
# 55 "menhir_parser.mly"
                                  ( ds )
# 278 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv343) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 42 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 286 "menhir_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv341) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 42 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 294 "menhir_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv339) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 42 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 302 "menhir_parser.ml"
            )) : (
# 42 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 306 "menhir_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv340)) : 'freshtv342)) : 'freshtv344)) : 'freshtv346)) : 'freshtv348)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv349 * _menhir_state * 'tv_declarations_with_loc) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)) : 'freshtv352)
    | _ ->
        _menhir_fail ()) : 'freshtv354)) : 'freshtv356)) : 'freshtv358)

and _menhir_goto_declarations_without_ending_semi_colon : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declarations_without_ending_semi_colon -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv321 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMI_COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AT_RULE _v ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | AT_RULE_WITHOUT_BODY _v ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | NESTED_AT_RULE _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EOF | RIGHT_BRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv313 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon), _startpos_ds_), _endpos__2_) = _menhir_stack in
            let _startpos = _startpos_ds_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 116 "menhir_parser.mly"
                                                            ( List.rev ds )
# 352 "menhir_parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv314)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv316)
    | EOF | RIGHT_BRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv317 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon), _startpos_ds_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_ds_ in
        let _v : 'tv_declarations = 
# 115 "menhir_parser.mly"
                                                ( List.rev ds )
# 368 "menhir_parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv318)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv319 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)) : 'freshtv322)

and _menhir_goto_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv311 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AT_RULE _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF | RIGHT_BRACE ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LEFT_BRACE ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv312)

and _menhir_goto_declaration_or_at_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_declaration_or_at_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv305 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos_ds_, _menhir_s, (ds : 'tv_declarations_without_ending_semi_colon), _startpos_ds_), _endpos__2_) = _menhir_stack in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon = 
# 121 "menhir_parser.mly"
                                                                                        ( d :: ds )
# 456 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv304)) : 'freshtv306)
    | MenhirState0 | MenhirState83 | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv309) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_declaration_or_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv307) = Obj.magic _menhir_stack in
        let (_endpos_d_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((d : 'tv_declaration_or_at_rule) : 'tv_declaration_or_at_rule) = _v in
        let (_startpos_d_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_d_ in
        let _endpos = _endpos_d_ in
        let _v : 'tv_declarations_without_ending_semi_colon = 
# 120 "menhir_parser.mly"
                               ( [d] )
# 477 "menhir_parser.ml"
         in
        _menhir_goto_declarations_without_ending_semi_colon _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv308)) : 'freshtv310)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_IMPORTANT_ : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_boption_IMPORTANT_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv301 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 489 "menhir_parser.ml"
    ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_boption_IMPORTANT_) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv299 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 498 "menhir_parser.ml"
    ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let ((i : 'tv_boption_IMPORTANT_) : 'tv_boption_IMPORTANT_) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let ((((_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 26 "menhir_parser.mly"
       (string)
# 506 "menhir_parser.ml"
    )), _startpos_n_), _, _, _startpos__2_), _endpos__3_, _startpos__3_), _endpos_v_, _, (v : 'tv_list_component_value_with_loc_), _startpos_v_) = _menhir_stack in
    let _startpos = _startpos_n_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_declaration = let _endpos = _endpos_i_ in
    let _startpos = _startpos_n_ in
    
# 130 "menhir_parser.mly"
                                                                                                   (
    { Declaration.name = (n, Lex_buffer.make_loc_and_fix _startpos_n_ _endpos_n_);
      value = (v, Lex_buffer.make_loc_and_fix _startpos_v_ _endpos_v_);
      important = (i, Lex_buffer.make_loc_and_fix _startpos_i_ _endpos_i_);
      loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
    }
  )
# 521 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv297) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv295) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv293) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : 'tv_declaration) : 'tv_declaration) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_declaration_or_at_rule = 
# 125 "menhir_parser.mly"
                    ( Declaration_list.Declaration d )
# 546 "menhir_parser.ml"
     in
    _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv294)) : 'freshtv296)) : 'freshtv298)) : 'freshtv300)) : 'freshtv302)

and _menhir_goto_style_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_style_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv291) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_style_rule) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv289) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : 'tv_style_rule) : 'tv_style_rule) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_rule = 
# 60 "menhir_parser.mly"
                   ( Rule.Style_rule r )
# 569 "menhir_parser.ml"
     in
    _menhir_goto_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv290)) : 'freshtv292)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_at_rule : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_at_rule -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState0 | MenhirState83 | MenhirState62 | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_at_rule) : 'tv_at_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_declaration_or_at_rule = 
# 126 "menhir_parser.mly"
                ( Declaration_list.At_rule r )
# 599 "menhir_parser.ml"
         in
        _menhir_goto_declaration_or_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv282)) : 'freshtv284)
    | MenhirState93 | MenhirState49 | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_at_rule) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv285) = Obj.magic _menhir_stack in
        let (_endpos_r_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((r : 'tv_at_rule) : 'tv_at_rule) = _v in
        let (_startpos_r_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_r_ in
        let _endpos = _endpos_r_ in
        let _v : 'tv_rule = 
# 59 "menhir_parser.mly"
                ( Rule.At_rule r )
# 620 "menhir_parser.ml"
         in
        _menhir_goto_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv286)) : 'freshtv288)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_component_value_with_loc_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 635 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv217 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 645 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv215 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 653 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 34 "menhir_parser.mly"
       (string)
# 659 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value = 
# 161 "menhir_parser.mly"
                                                                   (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_))
    )
# 669 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv216)) : 'freshtv218)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv219 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 679 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)) : 'freshtv222)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv225 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv223 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_component_value_with_loc), _startpos_x_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_component_value_with_loc_ = 
# 214 "<standard.mly>"
    ( x :: xs )
# 694 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv224)) : 'freshtv226)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv243 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_BRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv239 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv237 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_bracket_block = 
# 144 "menhir_parser.mly"
                                                                   ( xs )
# 717 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv235) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bracket_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((match _menhir_s with
            | MenhirState65 | MenhirState40 | MenhirState9 | MenhirState17 | MenhirState19 | MenhirState30 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv229) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bracket_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv227) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_bracket_block) : 'tv_bracket_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value = 
# 152 "menhir_parser.mly"
                      ( Component_value.Bracket_block b )
# 744 "menhir_parser.ml"
                 in
                _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv228)) : 'freshtv230)
            | MenhirState93 | MenhirState1 | MenhirState49 | MenhirState81 | MenhirState60 | MenhirState50 | MenhirState56 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv233) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_bracket_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv231) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_bracket_block) : 'tv_bracket_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value_in_prelude = 
# 177 "menhir_parser.mly"
                      ( Component_value.Bracket_block b )
# 765 "menhir_parser.ml"
                 in
                _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv232)) : 'freshtv234)
            | _ ->
                _menhir_fail ()) : 'freshtv236)) : 'freshtv238)) : 'freshtv240)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv241 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)) : 'freshtv244)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv261 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv255 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_paren_block = 
# 140 "menhir_parser.mly"
                                                               ( xs )
# 797 "menhir_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv253) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_paren_block) = _v in
            let (_startpos : Lexing.position) = _startpos in
            ((match _menhir_s with
            | MenhirState65 | MenhirState40 | MenhirState9 | MenhirState17 | MenhirState30 | MenhirState19 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv247) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_paren_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv245) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_paren_block) : 'tv_paren_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value = 
# 151 "menhir_parser.mly"
                    ( Component_value.Paren_block b )
# 824 "menhir_parser.ml"
                 in
                _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv246)) : 'freshtv248)
            | MenhirState93 | MenhirState1 | MenhirState49 | MenhirState81 | MenhirState60 | MenhirState56 | MenhirState50 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv251) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_paren_block) = _v in
                let (_startpos : Lexing.position) = _startpos in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
                let (_endpos_b_ : Lexing.position) = _endpos in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((b : 'tv_paren_block) : 'tv_paren_block) = _v in
                let (_startpos_b_ : Lexing.position) = _startpos in
                ((let _startpos = _startpos_b_ in
                let _endpos = _endpos_b_ in
                let _v : 'tv_component_value_in_prelude = 
# 176 "menhir_parser.mly"
                    ( Component_value.Paren_block b )
# 845 "menhir_parser.ml"
                 in
                _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv250)) : 'freshtv252)
            | _ ->
                _menhir_fail ()) : 'freshtv254)) : 'freshtv256)) : 'freshtv258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv269 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 862 "menhir_parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHT_PAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv265 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 872 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv263 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 880 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _endpos_f_, _menhir_s, (f : (
# 34 "menhir_parser.mly"
       (string)
# 886 "menhir_parser.ml"
            )), _startpos_f_), _endpos_xs_, _, (xs : 'tv_list_component_value_with_loc_), _startpos_xs_) = _menhir_stack in
            let _startpos = _startpos_f_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_component_value_in_prelude = 
# 187 "menhir_parser.mly"
                                                                   (
      Component_value.Function ((f, Lex_buffer.make_loc_and_fix _startpos_f_ _endpos_f_),
                                (xs, Lex_buffer.make_loc_and_fix _startpos_xs_ _endpos_xs_))
    )
# 896 "menhir_parser.ml"
             in
            _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv264)) : 'freshtv266)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv267 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 906 "menhir_parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv279 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 915 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IMPORTANT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv273) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271) = Obj.magic _menhir_stack in
            let (_endpos__1_ : Lexing.position) = _endpos in
            let (_startpos__1_ : Lexing.position) = _startpos in
            ((let _startpos = _startpos__1_ in
            let _endpos = _endpos__1_ in
            let _v : 'tv_boption_IMPORTANT_ = 
# 136 "<standard.mly>"
    ( true )
# 935 "menhir_parser.ml"
             in
            _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv272)) : 'freshtv274)
        | EOF | RIGHT_BRACE | SEMI_COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275) = Obj.magic _menhir_stack in
            ((let (_, _startpos) = Obj.magic _menhir_stack in
            let _endpos = _startpos in
            let _v : 'tv_boption_IMPORTANT_ = 
# 134 "<standard.mly>"
    ( false )
# 946 "menhir_parser.ml"
             in
            _menhir_goto_boption_IMPORTANT_ _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv277 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 956 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_list_component_value_with_loc_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_WHITESPACE_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_WHITESPACE_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState65 | MenhirState40 | MenhirState9 | MenhirState17 | MenhirState30 | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv207 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv195 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv193 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_component_value = 
# 159 "menhir_parser.mly"
                              ( Component_value.Delim ":" )
# 989 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv194)) : 'freshtv196)
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv199 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_component_value = 
# 160 "menhir_parser.mly"
                            ( Component_value.Delim "." )
# 1008 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv198)) : 'freshtv200)
        | HASH _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv203 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 35 "menhir_parser.mly"
       (string)
# 1018 "menhir_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv201 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos_h_ : Lexing.position) = _endpos in
            let ((h : (
# 35 "menhir_parser.mly"
       (string)
# 1028 "menhir_parser.ml"
            )) : (
# 35 "menhir_parser.mly"
       (string)
# 1032 "menhir_parser.ml"
            )) = _v in
            let (_startpos_h_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s, _, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_h_ in
            let _v : 'tv_component_value = 
# 165 "menhir_parser.mly"
                                 ( Component_value.Hash h )
# 1041 "menhir_parser.ml"
             in
            _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv202)) : 'freshtv204)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv205 * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)) : 'freshtv208)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv213 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 1056 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv209 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 1066 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DELIM _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DIMENSION _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FLOAT_DIMENSION _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FUNCTION _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_BRACKET ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LEFT_PAREN ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | NUMBER _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPERATOR _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNICODE_RANGE _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | URI _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | WHITESPACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState65 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | COLON | DOT | HASH _ ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | EOF | IMPORTANT | RIGHT_BRACE | SEMI_COLON ->
                _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv210)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv211 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 1115 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
    | _ ->
        _menhir_fail ()

and _menhir_goto_component_value : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv189) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_component_value) : 'tv_component_value) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_component_value_with_loc = let _endpos = _endpos_c_ in
    let _startpos = _startpos_c_ in
    
# 148 "menhir_parser.mly"
                        ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1143 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv187) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv185 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | EOF | IMPORTANT | RIGHT_BRACE | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv186)) : 'freshtv188)) : 'freshtv190)) : 'freshtv192)

and _menhir_goto_list_component_value_with_loc_in_prelude_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_component_value_with_loc_in_prelude_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState93 | MenhirState1 | MenhirState49 | MenhirState81 | MenhirState60 | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv179) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_list_component_value_with_loc_in_prelude_) : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_prelude = 
# 107 "menhir_parser.mly"
                                                 ( xs )
# 1213 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
        let (_endpos_xs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((xs : 'tv_prelude) : 'tv_prelude) = _v in
        let (_startpos_xs_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_xs_ in
        let _v : 'tv_prelude_with_loc = let _endpos = _endpos_xs_ in
        let _startpos = _startpos_xs_ in
        
# 103 "menhir_parser.mly"
               ( (xs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1239 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_prelude_with_loc) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        match _menhir_s with
        | MenhirState1 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv143 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 1253 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv139 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 1263 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AT_RULE _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DELIM _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DIMENSION _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | DOT ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FLOAT_DIMENSION _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | FUNCTION _v ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | HASH _v ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_BRACKET ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LEFT_PAREN ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NUMBER _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | OPERATOR _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | STRING _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UNICODE_RANGE _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | URI _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | WHITESPACE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHT_BRACE ->
                    _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | LEFT_BRACE ->
                    _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState49
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv140)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv141 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 1323 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)
        | MenhirState50 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv151 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 1332 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | SEMI_COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv147 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 1342 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv145 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 1350 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__3_ : Lexing.position) = _endpos in
                ((let ((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 32 "menhir_parser.mly"
       (string)
# 1356 "menhir_parser.ml"
                )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_) = _menhir_stack in
                let _startpos = _startpos_name_ in
                let _endpos = _endpos__3_ in
                let _v : 'tv_at_rule = let _endpos = _endpos__3_ in
                let _startpos = _startpos_name_ in
                
# 64 "menhir_parser.mly"
                                                                   (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Empty;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 1371 "menhir_parser.ml"
                 in
                _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv146)) : 'freshtv148)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv149 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 1381 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)) : 'freshtv152)
        | MenhirState60 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv157 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 1390 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv153 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 1400 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AT_RULE _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62) : 'freshtv154)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv155 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 1426 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)) : 'freshtv158)
        | MenhirState93 | MenhirState49 | MenhirState81 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv167 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AT_RULE _v ->
                    _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | AT_RULE_WITHOUT_BODY _v ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | NESTED_AT_RULE _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RIGHT_BRACE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv161 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_menhir_s : _menhir_state) = MenhirState83 in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv159 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos__3_ : Lexing.position) = _endpos in
                    let (_ : _menhir_state) = _menhir_s in
                    ((let ((_menhir_stack, _menhir_s, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__2_) = _menhir_stack in
                    let _startpos = _startpos_xs_ in
                    let _endpos = _endpos__3_ in
                    let _v : 'tv_style_rule = let _endpos = _endpos__3_ in
                    let _startpos = _startpos_xs_ in
                    
# 88 "menhir_parser.mly"
                                                   (
      { Style_rule.prelude = xs;
        block = [], Location.none;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 1475 "menhir_parser.ml"
                     in
                    _menhir_goto_style_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv160)) : 'freshtv162)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv164)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv165 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)) : 'freshtv168)
        | _ ->
            _menhir_fail ()) : 'freshtv170)) : 'freshtv172)) : 'freshtv174)) : 'freshtv176)) : 'freshtv178)) : 'freshtv180)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_component_value_with_loc_in_prelude_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
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
# 1510 "menhir_parser.ml"
         in
        _menhir_goto_list_component_value_with_loc_in_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv182)) : 'freshtv184)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_rule_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_list_rule_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rule_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
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
# 1538 "menhir_parser.ml"
         in
        _menhir_goto_list_rule_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv108)) : 'freshtv110)
    | MenhirState93 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_list_rule_) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
        let (_endpos_rs_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((rs : 'tv_list_rule_) : 'tv_list_rule_) = _v in
        let (_startpos_rs_ : Lexing.position) = _startpos in
        ((let _v : 'tv_stylesheet_without_eof = let _endpos = _endpos_rs_ in
        let _startpos = _startpos_rs_ in
        
# 51 "menhir_parser.mly"
                  ( (rs, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 1559 "menhir_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_stylesheet_without_eof) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState49 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv117 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 1572 "menhir_parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHT_BRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv113 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 1582 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv111 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 1590 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                let (_endpos__5_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _endpos_name_, _menhir_s, (name : (
# 31 "menhir_parser.mly"
       (string)
# 1596 "menhir_parser.ml"
                )), _startpos_name_), _, (xs : 'tv_prelude_with_loc), _startpos_xs_), _endpos__3_), _, (s : 'tv_stylesheet_without_eof)) = _menhir_stack in
                let _startpos = _startpos_name_ in
                let _endpos = _endpos__5_ in
                let _v : 'tv_at_rule = let _endpos = _endpos__5_ in
                let _startpos = _startpos_name_ in
                
# 71 "menhir_parser.mly"
                                                                                                      (
      { At_rule.name = (name, Lex_buffer.make_loc_and_fix _startpos_name_ _endpos_name_);
        prelude = xs;
        block = Brace_block.Stylesheet s;
        loc = Lex_buffer.make_loc_and_fix _startpos _endpos;
      }
    )
# 1611 "menhir_parser.ml"
                 in
                _menhir_goto_at_rule _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv112)) : 'freshtv114)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv115 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 1621 "menhir_parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)) : 'freshtv118)
        | MenhirState93 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EOF ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (s : 'tv_stylesheet_without_eof)) = _menhir_stack in
                let _v : (
# 41 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 1640 "menhir_parser.ml"
                ) = 
# 47 "menhir_parser.mly"
                                  ( s )
# 1644 "menhir_parser.ml"
                 in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv123) = _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 41 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 1652 "menhir_parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : (
# 41 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 1660 "menhir_parser.ml"
                )) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : (
# 41 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 1668 "menhir_parser.ml"
                )) : (
# 41 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 1672 "menhir_parser.ml"
                )) = _v in
                (Obj.magic _1 : 'freshtv120)) : 'freshtv122)) : 'freshtv124)) : 'freshtv126)) : 'freshtv128)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_stylesheet_without_eof) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
        | _ ->
            _menhir_fail ()) : 'freshtv134)) : 'freshtv136)) : 'freshtv138)
    | _ ->
        _menhir_fail ()

and _menhir_reduce52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_with_loc_ = 
# 212 "<standard.mly>"
    ( [] )
# 1694 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _v : 'tv_option_WHITESPACE_ = 
# 115 "<standard.mly>"
    ( None )
# 1704 "menhir_parser.ml"
     in
    _menhir_goto_option_WHITESPACE_ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv105) = Obj.magic _menhir_stack in
    let (_endpos_x_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos_x_ : Lexing.position) = _startpos in
    ((let x = () in
    let _startpos = _startpos_x_ in
    let _v : 'tv_option_WHITESPACE_ = 
# 117 "<standard.mly>"
    ( Some x )
# 1721 "menhir_parser.ml"
     in
    _menhir_goto_option_WHITESPACE_ _menhir_env _menhir_stack _menhir_s _v _startpos) : 'freshtv106)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 28 "menhir_parser.mly"
       (string)
# 1728 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv103) = Obj.magic _menhir_stack in
    let (_endpos_u_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((u : (
# 28 "menhir_parser.mly"
       (string)
# 1739 "menhir_parser.ml"
    )) : (
# 28 "menhir_parser.mly"
       (string)
# 1743 "menhir_parser.ml"
    )) = _v in
    let (_startpos_u_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_u_ in
    let _endpos = _endpos_u_ in
    let _v : 'tv_component_value = 
# 156 "menhir_parser.mly"
            ( Component_value.Uri u )
# 1751 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv104)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 37 "menhir_parser.mly"
       (string)
# 1758 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : (
# 37 "menhir_parser.mly"
       (string)
# 1769 "menhir_parser.ml"
    )) : (
# 37 "menhir_parser.mly"
       (string)
# 1773 "menhir_parser.ml"
    )) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_component_value = 
# 167 "menhir_parser.mly"
                      ( Component_value.Unicode_range r )
# 1781 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv102)

and _menhir_run13 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 1788 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
    let (_endpos_s_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 27 "menhir_parser.mly"
       (string)
# 1799 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 1803 "menhir_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_component_value = 
# 155 "menhir_parser.mly"
               ( Component_value.String s )
# 1811 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv100)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 29 "menhir_parser.mly"
       (string)
# 1818 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv97) = Obj.magic _menhir_stack in
    let (_endpos_o_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((o : (
# 29 "menhir_parser.mly"
       (string)
# 1829 "menhir_parser.ml"
    )) : (
# 29 "menhir_parser.mly"
       (string)
# 1833 "menhir_parser.ml"
    )) = _v in
    let (_startpos_o_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_o_ in
    let _endpos = _endpos_o_ in
    let _v : 'tv_component_value = 
# 157 "menhir_parser.mly"
                 ( Component_value.Operator o )
# 1841 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv98)

and _menhir_run15 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 36 "menhir_parser.mly"
       (string)
# 1848 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PERCENTAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 1860 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 1868 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 36 "menhir_parser.mly"
       (string)
# 1874 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_component_value = 
# 153 "menhir_parser.mly"
                           ( Component_value.Percentage n )
# 1881 "menhir_parser.ml"
         in
        _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv90)) : 'freshtv92)
    | COLON | DELIM _ | DIMENSION _ | DOT | EOF | FLOAT_DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | IMPORTANT | LEFT_BRACKET | LEFT_PAREN | NUMBER _ | OPERATOR _ | RIGHT_BRACE | RIGHT_BRACKET | RIGHT_PAREN | SEMI_COLON | STRING _ | UNICODE_RANGE _ | URI _ | WHITESPACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 1889 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 36 "menhir_parser.mly"
       (string)
# 1894 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_component_value = 
# 166 "menhir_parser.mly"
               ( Component_value.Number n )
# 1901 "menhir_parser.ml"
         in
        _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv94)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 1911 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 26 "menhir_parser.mly"
       (string)
# 1919 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv87) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 26 "menhir_parser.mly"
       (string)
# 1930 "menhir_parser.ml"
    )) : (
# 26 "menhir_parser.mly"
       (string)
# 1934 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value = 
# 154 "menhir_parser.mly"
              ( Component_value.Ident i )
# 1942 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv88)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "menhir_parser.mly"
       (string)
# 1949 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState19 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | RIGHT_PAREN ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 38 "menhir_parser.mly"
       (string * string * Types.dimension)
# 1994 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv85) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 38 "menhir_parser.mly"
       (string * string * Types.dimension)
# 2005 "menhir_parser.ml"
    )) : (
# 38 "menhir_parser.mly"
       (string * string * Types.dimension)
# 2009 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 168 "menhir_parser.mly"
                        ( Component_value.Float_dimension d )
# 2017 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv86)

and _menhir_run21 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 39 "menhir_parser.mly"
       (string * string)
# 2024 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv83) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 39 "menhir_parser.mly"
       (string * string)
# 2035 "menhir_parser.ml"
    )) : (
# 39 "menhir_parser.mly"
       (string * string)
# 2039 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 169 "menhir_parser.mly"
                  ( Component_value.Dimension d )
# 2047 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv84)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 30 "menhir_parser.mly"
       (string)
# 2054 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv81) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 30 "menhir_parser.mly"
       (string)
# 2065 "menhir_parser.ml"
    )) : (
# 30 "menhir_parser.mly"
       (string)
# 2069 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value = 
# 158 "menhir_parser.mly"
              ( Component_value.Delim d )
# 2077 "menhir_parser.ml"
     in
    _menhir_goto_component_value _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv82)

and _menhir_goto_component_value_in_prelude : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_component_value_in_prelude -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv79) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_in_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv77) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_component_value_in_prelude) : 'tv_component_value_in_prelude) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_c_ in
    let _endpos = _endpos_c_ in
    let _v : 'tv_component_value_with_loc_in_prelude = let _endpos = _endpos_c_ in
    let _startpos = _startpos_c_ in
    
# 173 "menhir_parser.mly"
                                   ( (c, Lex_buffer.make_loc_and_fix _startpos _endpos) )
# 2102 "menhir_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv75) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_component_value_with_loc_in_prelude) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv73 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE | SEMI_COLON ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv74)) : 'freshtv76)) : 'freshtv78)) : 'freshtv80)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 26 "menhir_parser.mly"
       (string)
# 2158 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState63 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv38)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv39 * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * Lexing.position * _menhir_state * 'tv_rule * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv43 * Lexing.position * _menhir_state * 'tv_declarations_without_ending_semi_colon * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv45 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 2201 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_option_WHITESPACE_ * Lexing.position) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * Lexing.position * _menhir_state * (
# 26 "menhir_parser.mly"
       (string)
# 2210 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2219 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * Lexing.position * _menhir_state * (
# 33 "menhir_parser.mly"
       (string)
# 2228 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * Lexing.position * _menhir_state * 'tv_component_value_with_loc_in_prelude * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * Lexing.position * _menhir_state * (
# 32 "menhir_parser.mly"
       (string)
# 2242 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 2251 "menhir_parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_prelude_with_loc * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv59 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2260 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * Lexing.position * _menhir_state * 'tv_component_value_with_loc * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv63 * Lexing.position * _menhir_state * (
# 34 "menhir_parser.mly"
       (string)
# 2274 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * Lexing.position * _menhir_state * (
# 31 "menhir_parser.mly"
       (string)
# 2293 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv71) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv72)

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_component_value_with_loc_in_prelude_ = 
# 212 "<standard.mly>"
    ( [] )
# 2309 "menhir_parser.ml"
     in
    _menhir_goto_list_component_value_with_loc_in_prelude_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : 'tv_list_rule_ = 
# 212 "<standard.mly>"
    ( [] )
# 2320 "menhir_parser.ml"
     in
    _menhir_goto_list_rule_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_component_value_in_prelude = 
# 184 "menhir_parser.mly"
               ( Component_value.Delim "*" )
# 2337 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv36)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 28 "menhir_parser.mly"
       (string)
# 2344 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv33) = Obj.magic _menhir_stack in
    let (_endpos_u_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((u : (
# 28 "menhir_parser.mly"
       (string)
# 2355 "menhir_parser.ml"
    )) : (
# 28 "menhir_parser.mly"
       (string)
# 2359 "menhir_parser.ml"
    )) = _v in
    let (_startpos_u_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_u_ in
    let _endpos = _endpos_u_ in
    let _v : 'tv_component_value_in_prelude = 
# 181 "menhir_parser.mly"
            ( Component_value.Uri u )
# 2367 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv34)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 37 "menhir_parser.mly"
       (string)
# 2374 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv31) = Obj.magic _menhir_stack in
    let (_endpos_r_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((r : (
# 37 "menhir_parser.mly"
       (string)
# 2385 "menhir_parser.ml"
    )) : (
# 37 "menhir_parser.mly"
       (string)
# 2389 "menhir_parser.ml"
    )) = _v in
    let (_startpos_r_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_r_ in
    let _endpos = _endpos_r_ in
    let _v : 'tv_component_value_in_prelude = 
# 193 "menhir_parser.mly"
                      ( Component_value.Unicode_range r )
# 2397 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv32)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 27 "menhir_parser.mly"
       (string)
# 2404 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv29) = Obj.magic _menhir_stack in
    let (_endpos_s_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((s : (
# 27 "menhir_parser.mly"
       (string)
# 2415 "menhir_parser.ml"
    )) : (
# 27 "menhir_parser.mly"
       (string)
# 2419 "menhir_parser.ml"
    )) = _v in
    let (_startpos_s_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : 'tv_component_value_in_prelude = 
# 180 "menhir_parser.mly"
               ( Component_value.String s )
# 2427 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv30)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 29 "menhir_parser.mly"
       (string)
# 2434 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv27) = Obj.magic _menhir_stack in
    let (_endpos_o_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((o : (
# 29 "menhir_parser.mly"
       (string)
# 2445 "menhir_parser.ml"
    )) : (
# 29 "menhir_parser.mly"
       (string)
# 2449 "menhir_parser.ml"
    )) = _v in
    let (_startpos_o_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_o_ in
    let _endpos = _endpos_o_ in
    let _v : 'tv_component_value_in_prelude = 
# 182 "menhir_parser.mly"
                 ( Component_value.Operator o )
# 2457 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv28)

and _menhir_run7 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 36 "menhir_parser.mly"
       (string)
# 2464 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PERCENTAGE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 2476 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 2484 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 36 "menhir_parser.mly"
       (string)
# 2490 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_component_value_in_prelude = 
# 178 "menhir_parser.mly"
                           ( Component_value.Percentage n )
# 2497 "menhir_parser.ml"
         in
        _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv20)) : 'freshtv22)
    | COLON | DELIM _ | DIMENSION _ | DOT | FLOAT_DIMENSION _ | FUNCTION _ | HASH _ | IDENT _ | LEFT_BRACE | LEFT_BRACKET | LEFT_PAREN | NUMBER _ | OPERATOR _ | SEMI_COLON | STRING _ | UNICODE_RANGE _ | URI _ | WHITESPACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 2505 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_n_, _menhir_s, (n : (
# 36 "menhir_parser.mly"
       (string)
# 2510 "menhir_parser.ml"
        )), _startpos_n_) = _menhir_stack in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_component_value_in_prelude = 
# 192 "menhir_parser.mly"
               ( Component_value.Number n )
# 2517 "menhir_parser.ml"
         in
        _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv24)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * Lexing.position * _menhir_state * (
# 36 "menhir_parser.mly"
       (string)
# 2527 "menhir_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 31 "menhir_parser.mly"
       (string)
# 2535 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run9 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
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
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | RIGHT_PAREN ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run17 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | RIGHT_BRACKET ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run38 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 26 "menhir_parser.mly"
       (string)
# 2666 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 26 "menhir_parser.mly"
       (string)
# 2677 "menhir_parser.ml"
    )) : (
# 26 "menhir_parser.mly"
       (string)
# 2681 "menhir_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_i_ in
    let _endpos = _endpos_i_ in
    let _v : 'tv_component_value_in_prelude = 
# 179 "menhir_parser.mly"
              ( Component_value.Ident i )
# 2689 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv18)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 35 "menhir_parser.mly"
       (string)
# 2696 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
    let (_endpos_h_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((h : (
# 35 "menhir_parser.mly"
       (string)
# 2707 "menhir_parser.ml"
    )) : (
# 35 "menhir_parser.mly"
       (string)
# 2711 "menhir_parser.ml"
    )) = _v in
    let (_startpos_h_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_h_ in
    let _endpos = _endpos_h_ in
    let _v : 'tv_component_value_in_prelude = 
# 191 "menhir_parser.mly"
             ( Component_value.Hash h )
# 2719 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv16)

and _menhir_run40 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 34 "menhir_parser.mly"
       (string)
# 2726 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DELIM _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON | DOT | HASH _ ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | RIGHT_PAREN ->
        _menhir_reduce52 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run43 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 38 "menhir_parser.mly"
       (string * string * Types.dimension)
# 2771 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 38 "menhir_parser.mly"
       (string * string * Types.dimension)
# 2782 "menhir_parser.ml"
    )) : (
# 38 "menhir_parser.mly"
       (string * string * Types.dimension)
# 2786 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value_in_prelude = 
# 194 "menhir_parser.mly"
                        ( Component_value.Float_dimension d )
# 2794 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv14)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_component_value_in_prelude = 
# 186 "menhir_parser.mly"
        ( Component_value.Delim "." )
# 2811 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv12)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 39 "menhir_parser.mly"
       (string * string)
# 2818 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 39 "menhir_parser.mly"
       (string * string)
# 2829 "menhir_parser.ml"
    )) : (
# 39 "menhir_parser.mly"
       (string * string)
# 2833 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value_in_prelude = 
# 195 "menhir_parser.mly"
                  ( Component_value.Dimension d )
# 2841 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv10)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 30 "menhir_parser.mly"
       (string)
# 2848 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
    let (_endpos_d_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((d : (
# 30 "menhir_parser.mly"
       (string)
# 2859 "menhir_parser.ml"
    )) : (
# 30 "menhir_parser.mly"
       (string)
# 2863 "menhir_parser.ml"
    )) = _v in
    let (_startpos_d_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : 'tv_component_value_in_prelude = 
# 183 "menhir_parser.mly"
              ( Component_value.Delim d )
# 2871 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv8)

and _menhir_run47 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_component_value_in_prelude = 
# 185 "menhir_parser.mly"
          ( Component_value.Delim ":" )
# 2888 "menhir_parser.ml"
     in
    _menhir_goto_component_value_in_prelude _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv6)

and _menhir_run50 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 32 "menhir_parser.mly"
       (string)
# 2895 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | SEMI_COLON ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run60 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 33 "menhir_parser.mly"
       (string)
# 2944 "menhir_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACE ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState60
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
# 42 "menhir_parser.mly"
       (Types.Declaration_list.t)
# 3005 "menhir_parser.ml"
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
    | AT_RULE _v ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv4))

and stylesheet : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 41 "menhir_parser.mly"
       (Types.Stylesheet.t)
# 3035 "menhir_parser.ml"
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
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | AT_RULE_WITHOUT_BODY _v ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COLON ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DELIM _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DIMENSION _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOT ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FLOAT_DIMENSION _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FUNCTION _v ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | HASH _v ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_BRACKET ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LEFT_PAREN ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NESTED_AT_RULE _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | NUMBER _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | OPERATOR _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UNICODE_RANGE _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | URI _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | WHITESPACE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState93 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | LEFT_BRACE ->
        _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState93
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv2))

# 270 "<standard.mly>"
  

# 3099 "menhir_parser.ml"
