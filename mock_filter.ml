module Make (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters

  let rec handle_body body (function_name, arguments) =
    match body with
    | Ast.TyArr (_loc, ( Ast.TyId (_, (Ast.IdLid ( _, argument_left)))),( Ast.TyId (_, (Ast.IdLid ( _, argument_right))))) ->
        (function_name,  List.append arguments [argument_left;argument_right;])
    |Ast.TyArr (_loc, ( Ast.TyId (_, (Ast.IdLid ( _, argument_left)))), ty_arr) ->
        handle_body ty_arr (function_name, List.append arguments [argument_left])
    |_ ->
        (function_name,arguments)


  let rec handle_function func_decl =
    match func_decl with
    | Ast.SgVal (_loc, function_name, body) ->
        let function_name,arguments = handle_body body (function_name,[]) in
        (function_name,arguments)
    |func_decl -> ("onbekend",[])

    let rec filter_sg_sem sg list_of_func_desc =
      match sg with
        | Ast.SgSem (_loc, function_left, Ast.SgSem (_, function_right, rest_of_functions)) ->
            let func_desc1 = handle_function function_left in
            let func_desc2 = handle_function function_right in
            filter_sg_sem rest_of_functions (List.append list_of_func_desc [func_desc1;func_desc2;])

        | Ast.SgSem (_loc, function_left, function_right) ->
            let func_desc1 = handle_function function_left in
            let func_desc2 = handle_function function_right in
            List.append list_of_func_desc [func_desc1;func_desc2;]
        | Ast.SgVal (_, _, _) ->
            let func_desc = handle_function sg in
            List.append list_of_func_desc [func_desc]
        |_ -> list_of_func_desc

    let print_function_signatures signatures =
      let print_sig (function_name,args) =
        let () = Pervasives.print_string ("function name is :"^function_name ^"\n") in
        let () = Pervasives.print_string "arguments are : " in
        List.map (fun s -> Pervasives.print_string (s^"\n")) args
      in
      List.map (fun signature ->
        let () = Pervasives.print_string "signature is :\n" in
        let _ = print_sig signature in
        Pervasives.print_string "\n"
        ) signatures

   module TypeUtil =
     struct
        let type_in_module _loc module_name type_name =
          Ast.TyId(_loc, (Ast.IdAcc (_loc, (Ast.IdUid (_loc, module_name)), (Ast.IdLid (_loc, type_name)))))

        let create_tuple_type_from_string_list _loc types =
          let () =if List.length types < 2 then
            Pervasives.invalid_arg "need at least two arguments to create a tuple"
          in
          let ex_ids = List.map (fun s -> Ast.TyId (_loc, (Ast.IdLid (_loc, s)))) types in
          let reversed_ex_ids = List.rev ex_ids in
          let last = List.hd reversed_ex_ids in
          let rest = List.tl reversed_ex_ids in
          let pre_last = List.hd rest in
          let rest = List.tl rest in
          let base_tuple = Ast.TySta (_loc, pre_last, last) in
          let tuple = List.fold_left (fun ex_id ex_com -> Ast.TySta (_loc, ex_id, ex_com))  base_tuple rest in
          Ast.TyTup(_loc,tuple)

        let apply _loc type_type arg_types =
          let first_type = List.hd arg_types in
          let rest = List.tl arg_types in
          let base_application = Ast.TyApp (_loc, type_type, first_type) in
          List.fold_left (fun arg_type application-> Ast.TyApp (_loc, application, arg_type))  base_application rest

       let create_hashtbl_type _loc type1 type2 =
	Ast.TyApp (_loc,  (Ast.TyApp (_loc,
			      (Ast.TyId (_loc,
				 (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Hashtbl")),
				    (Ast.IdLid (_loc, "t")))))),   type1)),
			   type2)
     end


    module ExpressionUtil =
      struct
        let call_in_module _loc module_name function_name =
          let func_expr = <:expr< $lid:function_name$ >> in
          let module_expr = <:expr< $uid:module_name$ >> in
          <:expr< $module_expr$ . $func_expr$ >>

        let successor_func _loc =
          call_in_module _loc "Pervasives" "succ"

        let apply _loc func_expr arg_exprs =
          let first_arg = List.hd arg_exprs in
          List.fold_left
            (fun func_expr arg -> <:expr< $func_expr$ $arg$ >> )
            <:expr< $func_expr$ $first_arg$ >>
            (List.tl arg_exprs)

        let create_let_expression _loc assign_pat assign_expr in_expr =
          <:expr< let $assign_pat$ = $assign_expr$ in $in_expr$ >>

        let create_simple_expr _loc s =
          <:expr< $lid:s$ >>

        let build_function _loc arguments body =
          List.fold_right (fun arg expr -> <:expr< fun $lid:arg$ -> $expr$ >> ) arguments body

        let assign _loc name value  =
          <:expr< $name$ = $value$ >>

        let assign_mutable _loc name value  =
          <:expr< $name$ := $value$ >>

        let exp_to_str_item _loc expression =
          <:str_item< $exp:expression$ >>

        let bang _loc = call_in_module _loc "Pervasives" "!"
        let unref _loc expr =
          apply _loc (bang _loc) [expr;]

        let create_tuple_from_string_list _loc types =
          let () =if List.length types < 2 then
            Pervasives.invalid_arg "need at least two arguments to create a tuple"
          in
          let ex_ids = List.map (fun s -> Ast.ExId (_loc, (Ast.IdLid (_loc, s)))) types in
          let reversed_ex_ids = List.rev ex_ids in
          let last = List.hd reversed_ex_ids in
          let rest = List.tl reversed_ex_ids in
          let pre_last = List.hd rest in
          let rest = List.tl rest in
          let base_tuple = Ast.ExCom (_loc, pre_last, last) in
          let tuple = List.fold_left (fun ex_id ex_com -> Ast.ExCom (_loc, ex_id, ex_com))  base_tuple rest in
          Ast.ExTup(_loc,tuple)

      end


    module PatternUtil =
      struct
        let unit_patt _loc =
          let a = "()" in
          <:patt< $lid:a$ >>

        let create_pattern _loc patt =
          <:patt< $lid:patt$ >>
      end

    let create_base_assert_function _loc =
    (*  let equals_func =  ExpressionUtil.call_in_module _loc "Eqx" "eq_int" in
      let expr_s = ExpressionUtil.create_simple_expr _loc "s" in
      let expr_expected = ExpressionUtil.create_simple_expr _loc "expected" in
      let expr_interactions = ExpressionUtil.create_simple_expr _loc "interactions" in
      let equals_func = ExpressionUtil.apply _loc equals_func [expr_s;expr_expected;expr_interactions;] in

      let sprintf_func = ExpressionUtil.call_in_module _loc "Printf" "sprintf" in
      let name= ExpressionUtil.create_simple_expr _loc "name" in
      let sprintf_string = <:expr< "interactions with %s" >> in
      let printf_func = ExpressionUtil.apply _loc sprintf_func [sprintf_string;name;] in

      let patt_s = PatternUtil.create_pattern _loc "s" in
      let expression = ExpressionUtil.create_let_expression _loc patt_s printf_func equals_func in
      let base_assert = ExpressionUtil.build_function  _loc ["name";"expected";"interactions";] expression in
      <:str_item< let base_assert = $exp:base_assert$ >>*)
    <:str_item< let base_assert name expected interactions =
      let s = Printf.sprintf "interactions with %s" name in
      let msg = Printf.sprintf "%s expected: %d actual: %d" s expected !interactions in
      OUnit.assert_equal ~msg expected !interactions >>


    let create_base_incrementation_function _loc =
      let interactions = "interactions" in
      let interactions = ExpressionUtil.create_simple_expr _loc interactions in
      let interactions_unrefed = ExpressionUtil.unref _loc interactions in
      let interactions_incremented = ExpressionUtil.apply _loc (ExpressionUtil.successor_func _loc) [interactions_unrefed;] in
      let base_incrementation = ExpressionUtil.assign_mutable _loc interactions interactions_incremented in
      let base_incrementation =  ExpressionUtil.build_function _loc ["interactions";] base_incrementation in
      <:str_item< let base_incrementation = $exp:base_incrementation$ >>

    let create_assertions _loc function_names =
      let all_function_names = List.map (fun s -> ("assert_"^s^"_interactions", s,s^"_interactions")) function_names in
         Ast.stSem_of_list (
          List.map (fun (assert_function_name,function_name, function_interaction) ->
            <:str_item< $(Ast.StVal (_loc, Ast.ReNil,
              (Ast.BiEq (_loc,
                (Ast.PaId (_loc, (Ast.IdLid (_loc, assert_function_name)))),
                  (Ast.ExFun (_loc,  (Ast.McArr (_loc,
                    (Ast.PaId (_loc, (Ast.IdLid (_loc, "expected")))),  (Ast.ExNil _loc), (Ast.ExApp (_loc, (Ast.ExApp (_loc, (Ast.ExApp (_loc,
                    (Ast.ExId (_loc, (Ast.IdLid (_loc, "base_assert")))),   (Ast.ExStr (_loc, function_name)))), (Ast.ExId (_loc, (Ast.IdLid (_loc, "expected")))))),
                    (Ast.ExId (_loc,(Ast.IdLid (_loc, function_interaction ))))))))))))))$ >>) all_function_names)

    let create_hashtables_for_arguments _loc function_list=
      let hashtbls = List.map (fun (function_name, arguments_with_return_type) ->
	let function_arguments = List.rev (List.tl (List.rev arguments_with_return_type)) in
        let hashtbl_create = ExpressionUtil.call_in_module _loc "Hashtbl" "create" in
        let drie = "3" in
        let drie = <:expr< $int:drie$ >> in
        let hashtbl_create = ExpressionUtil.apply _loc hashtbl_create [drie;] in
        let hashtbl_name = function_name^"_hashtbl" in
        let hashtbl_name = PatternUtil.create_pattern _loc hashtbl_name in
        let argument_tuple = if List.length function_arguments > 1 then
		 TypeUtil.create_tuple_type_from_string_list _loc function_arguments
		else
                 let arg = List.hd function_arguments in
		 Ast.TyId(_loc, (Ast.IdLid (_loc, arg)))
	 in
        let argument_type = Ast.TyId (_loc, (Ast.IdLid (_loc, "int"))) in
	let hashtbl_type = TypeUtil.create_hashtbl_type _loc argument_type argument_tuple in
(*        let hashtbl_create = <:expr< $hashtbl_create$ : $hashtbl_type$ >> in*) 
        let type_constrained_hashtbl = Ast.ExTyc (_loc,hashtbl_create,hashtbl_type) in
        <:str_item< let $hashtbl_name$ = $exp:type_constrained_hashtbl$  >>
      ) function_list in
        Ast.stSem_of_list hashtbls

    let create_reference_counters _loc function_names =
      let function_names = List.map (fun s -> s^"_interactions") function_names in
         Ast.stSem_of_list (
        List.map (fun function_name ->
            <:str_item< $(Ast.StVal (_loc, Ast.ReNil,
              Ast.BiEq (_loc,
                (Ast.PaId (_loc,  (Ast.IdLid (_loc, function_name)))),
                (Ast.ExApp (_loc,
                  (Ast.ExId (_loc, (Ast.IdLid (_loc, "ref")))),
                  (Ast.ExInt (_loc, "0")))))))$ >>) function_names)

    let create_return_values _loc functions =
      let function_and_ret_val = List.map (fun (name,args) -> (name^"_ret_val", List.hd (List.rev args))) functions in
      List.map (fun (name, ret_val) -> let ret_val = <:ctyp< $lid:ret_val$ >> in  <:sig_item< val $name$ : $ret_val$ >> ) function_and_ret_val

    let create_body_for_function _loc name arguments_only =
      let ret_val = ExpressionUtil.call_in_module _loc "Behaviour" (name^"_ret_val") in
      let interactions = name ^ "_interactions" in
      let interactions = <:expr< $lid:interactions$ >> in
      let hashtbl_name = name^"_hashtbl" in
      let next_interaction = ExpressionUtil.apply _loc (ExpressionUtil.create_simple_expr _loc "base_incrementation") [interactions;] in
      let unit_patt = PatternUtil.unit_patt _loc in
      let incrementation_expression = ExpressionUtil.create_let_expression _loc unit_patt next_interaction ret_val in
      let hashtbl_add_func = ExpressionUtil.call_in_module _loc "Hashtbl" "add" in
      let interactions_unrefed = ExpressionUtil.unref _loc interactions in
(*      let argument_expressions = List.map (fun arg -> ExpressionUtil.create_simple_expr _loc arg ) arguments_only in*)
      let aggregated_args = if (List.length arguments_only > 1) then ExpressionUtil.create_tuple_from_string_list _loc arguments_only else ExpressionUtil.create_simple_expr _loc (List.hd arguments_only) in
      let hashtbl_add = ExpressionUtil.apply _loc hashtbl_add_func [ExpressionUtil.create_simple_expr _loc hashtbl_name;interactions_unrefed;aggregated_args] in
      ExpressionUtil.create_let_expression _loc unit_patt hashtbl_add incrementation_expression

    let create_mocked_functions _loc list_of_functions =
      let list_of_mock_functions =
        List.map (fun (name, argument_types_and_return_type) ->
          let argument_types_only = (List.rev (List.tl (List.rev argument_types_and_return_type))) in
          let end_counter_and_arguments = List.fold_left (fun (counter,argument_names) arg -> ( succ counter, (arg^(Pervasives.string_of_int counter))::argument_names)) (1,[]) argument_types_only in
          let arguments_only = List.rev (Pervasives.snd end_counter_and_arguments) in
          let body = create_body_for_function _loc name arguments_only in
          let expr = ExpressionUtil.build_function  _loc arguments_only body in
          <:str_item< let $lid:name$ = $expr$ >>
        ) list_of_functions in
        Ast.stSem_of_list list_of_mock_functions


    let create_behaviour_type_module _loc list_of_function_signatures =
      let sig_list_behaviour = create_return_values _loc list_of_function_signatures in
      <:module_type< sig $list:sig_list_behaviour$ end >>

    let create_stub_module _loc module_name behaviour_type list_of_function_signatures =
      let function_names = List.map Pervasives.fst list_of_function_signatures in
      let base_assert_function = create_base_assert_function _loc in
      let base_incrementation_function = create_base_incrementation_function _loc in
      let reference_counter_str_item = create_reference_counters _loc function_names in
      let hashtbls_str_item = create_hashtables_for_arguments _loc list_of_function_signatures in
      let assertion_str_item = create_assertions _loc function_names in
      let mocked_functions = create_mocked_functions _loc list_of_function_signatures in
      let body_list = [reference_counter_str_item;hashtbls_str_item;base_assert_function;base_incrementation_function;assertion_str_item;mocked_functions;] in
      let functor_module_name = "Behaviour" in
      let name = "TestStubFunctorBehaviourSig" in
      let module_expr = <:module_expr< functor ( $functor_module_name$ : $uid:name$ ) -> struct $list:body_list$ end >> in
      <:str_item< module $uid:module_name$ = $module_expr$ >>

    let rec filter si =
      match si with
        | Ast.StMty (_loc, name, (Ast.MtSig (_, function_signatures))) ->
            let list_of_function_signatures  = filter_sg_sem function_signatures [] in
(*            let _ = print_function_signatures list_of_function_signatures in*)
            let module_name = "TestStubFunctor" in
            let module_name_behaviour =  module_name^"BehaviourSig" in
            let module_type_behaviour = create_behaviour_type_module _loc list_of_function_signatures in
            let module_type_str_item = <:str_item< module type $module_name_behaviour$ = $module_type_behaviour$ >> in
            let module_str_item = create_stub_module _loc module_name module_type_behaviour list_of_function_signatures in
            Ast.stSem_of_list [module_type_str_item;module_str_item;]
        | si ->
          let _loc = Ast.loc_of_str_item si in
          <:str_item<  >>
          ;;

  AstFilters.register_str_item_filter begin fun si ->
    let _loc = Ast.loc_of_str_item si in
    <:str_item<
      $list: List.map filter (Ast.list_of_str_item si [])$
    >>
  end
  end
module Id =
struct
  let name = "mock_filter"
  let version = "0.1"
end
;;
let module M = Camlp4.Register.AstFilter(Id)(Make) in ()
