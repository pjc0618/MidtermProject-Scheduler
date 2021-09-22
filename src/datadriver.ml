open Yojson.Basic
open Yojson.Basic.Util
open Parser
open Task
open Unix
open ANSITerminal

type variable = 
  | NextId of string * int

type json_file = string

exception InvalidId of int
exception DuplicateId of int
exception DuplicateDescription of string
exception DuplicateTag of string
exception InvalidDescription of string
exception InvalidTag of string
exception InvalidVariable of string
exception InvalidJsonFormat of string

(** [get_tree file] is the root node of the json tree of file [file]. *)
let get_tree (file:json_file): (Yojson.Basic.json) = 
  from_file file

(** [get_tasks_t file] is a list of task json trees from file [file].
    Raises: [InvalidJsonFormat] if [file] is not in the right format. *)
let get_tasks_t (file:json_file) : Yojson.Basic.json list = 
  try member "tasks" (get_tree file) |> Util.to_list
  with (Util.Type_error(x,_)) -> raise (InvalidJsonFormat x)

(** [get_vars_t file] is a list of variables json trees from file [file].
    Raises: [InvalidJsonFormat] if [file] is not in the right format. *)
let get_vars_t (file:json_file) : Yojson.Basic.json list = 
  try member "variables" (get_tree file) |> Util.to_list
  with (Util.Type_error(x,_)) -> raise (InvalidJsonFormat x)

(** [get_task_from_json_obj obj] is a [Task.t] with the properties defined by
    the json object [obj]. Raises: [InvalidJsonFormat] if [obj] is not a json
    object following the required task format. *)
let get_task_from_json_obj (obj:Yojson.Basic.json) : Task.t = 
  try let task_id = member "id" obj |> Util.to_int in 
    let task_desc = member "description" obj |> Util.to_string in 
    let task_completed = member "completed" obj |> Util.to_bool in
    let deadline_assoc = member "deadline" obj in
    let deadline_set = deadline_assoc |> member "set" |> Util.to_bool in
    let task_deadline = deadline_assoc |> member "value" |> Util.to_float in
    let tags_list = member "tags" obj |> Util.to_list
                    |> List.map (fun a -> member "tagid" a |> Util.to_string) in
    let priority = member "priority" obj |>Util.to_int in

    if deadline_set then
      Task.make task_id task_desc task_completed (Some(task_deadline)) tags_list priority
    else Task.make task_id task_desc task_completed None tags_list priority

  (*BISECT-IGNORE*) with (Util.Type_error(x,_)) -> raise (InvalidJsonFormat x)

(** [get_json_obj_from_task task] is a Yojson tree version of [task]. *)
let get_json_obj_from_task (task:Task.t) : Yojson.Basic.json = 
  let task_id = Task.id task in
  let task_desc = Task.description task in
  let task_completed = Task.completed task in
  let deadline_option = Task.deadline task in
  let deadline_set = match deadline_option with
      Some _ -> true | None -> false in
  let deadline = match deadline_option with
      Some v -> v | None -> 0. in
  let tags = Task.tags task in
  let tags_as_assoc_list = List.map (fun t -> 
      `Assoc([("tagid", `String(t))])) tags in
  let priority = Task.priority task in

  `Assoc([
      ("id", `Int(task_id));
      ("description", `String(task_desc));
      ("completed", `Bool(task_completed));
      ("deadline", `Assoc([
           ("set", `Bool(deadline_set));
           ("value", `Float(deadline));
         ]));
      ("tags", `List(tags_as_assoc_list));
      ("priority",`Int(priority)) ])

(** [get_json_obj_list_from_tasks tasks] is a list of json objects representing
    tasks in [tasks]. *)
let get_json_obj_list_from_tasks (tasks:Task.t list) : Yojson.Basic.json list = 
  List.map (fun t -> get_json_obj_from_task t) tasks

let get_variable_from_json_obj (obj:Yojson.Basic.json) : variable = 
  try let variable_identifier = member "identifier" obj |> Util.to_string in
    match variable_identifier with
    | "next_id" -> member "value" obj |> Util.to_int |> 
                   (fun x -> (NextId("next_id", x)))
    (*BISECT-IGNORE-BEGIN*)
    | _ -> raise (InvalidJsonFormat "")
  with (Util.Type_error(x,_)) -> raise (InvalidJsonFormat x)
(*BISECT-IGNORE-END*)

let get_json_obj_from_variable (var:variable) : Yojson.Basic.json = 
  let NextId(id,v) = var in 

  `Assoc([
      ("identifier", `String(id));
      ("value", `Int(v))
    ])

let get_json_obj_list_from_vars (vars:variable list) : Yojson.Basic.json list =
  List.map (fun t -> get_json_obj_from_variable t) vars

(** [get_file_assoc vars tasks] is the root node of a json data file constructed
    from variables [vars] and tasks [tasks]. *)
let get_file_assoc (vars:Yojson.Basic.json list) (tasks:Yojson.Basic.json list)
  : Yojson.Basic.json =
  `Assoc([
      ("variables", `List(vars));
      ("tasks", `List(tasks))])

(** [write_to_file file vars tasks] writes a json tree constructed from [vars]
    and [tasks] to [file]. *)
let write_to_file (file:json_file) (vars:Yojson.Basic.json list) 
    (tasks:Yojson.Basic.json list) : unit =
  to_file file (get_file_assoc vars tasks)

let get_all_tasks (file:json_file) : Task.t list = 
  get_tasks_t file |> List.map (fun t -> get_task_from_json_obj t)

let filter_tasks (file:json_file) (p:(Task.t -> bool)) : Task.t list =
  get_all_tasks file |> List.filter p

(** [ensure_exists file p exc] is the first task in [file] that satisfies
    predicate [p]. If no such task exists, raise [exc]. *)
let ensure_exists (file:json_file) (p:(Task.t -> bool)) (exc:exn) : Task.t =
  let tasks_satisfying_p = filter_tasks file p in
  if List.length tasks_satisfying_p < 1 then raise exc
  else List.hd tasks_satisfying_p

let get_task_by_id (file:json_file) (id:int) : Task.t = 
  ensure_exists file (fun t -> Task.id t = id) (InvalidId(id))

(** [task_exists_with_id file id] is whether there exists a task with id [id]
    in
    [file]. *)
let task_exists_with_id (file:json_file) (id:int) : bool =
  try ignore(get_task_by_id file id); true
  with | InvalidId(id) -> false

let get_tasks_by_tag (file:json_file) (tag:string) : Task.t list =
  filter_tasks file (fun t -> Task.tags t |> List.mem tag)

let get_task_id_by_description (file:json_file) (desc:string) : int =
  ensure_exists file (fun t -> Task.description t = desc) 
    (InvalidDescription(desc)) |> Task.id

let task_exists_with_description (file:json_file) (desc:string) : bool =
  try ignore(get_task_id_by_description file desc); true
  with InvalidDescription(desc) -> false

let remove_task_by_id (file:json_file) (id:int) : unit = 
  if not(task_exists_with_id file id) then raise (InvalidId(id))
  else 
    let new_tasks_list = filter_tasks file (fun t -> not(Task.id t = id))
                         |> get_json_obj_list_from_tasks in
    write_to_file file (get_vars_t file) (new_tasks_list)

(** [contains_whitespace_or_hyphen str] is whether [str] contains a whitespace
    or hyphen character. *)
let contains_whitespace_or_hyphen (str:string) : bool =
  String.contains str '-' || String.contains str ' '
  || String.contains str '\t' || String.contains str '\n'

let update_property_by_id (file:json_file) (id:int)
    (property:Task.updatable_property) : unit = 
  if not(task_exists_with_id file id) then raise (InvalidId(id))
  else
    let success (file:json_file) (new_task_json_list:Yojson.Basic.json list) =
      write_to_file file (get_vars_t file) (new_task_json_list) in
    let new_task_json_list = get_all_tasks file
                             |> List.map (fun t -> 
                                 if Task.id t = id then 
                                   Task.update_property t property
                                 else t) |> get_json_obj_list_from_tasks in
    match property with
    | Description(s) -> if task_exists_with_description file s 
      then raise ((DuplicateDescription(s)))
      else success file new_task_json_list
    | Tags(t) -> begin 
        ignore(List.map (fun s -> (if contains_whitespace_or_hyphen s 
                                   then raise (InvalidTag(s))
                                   else ()))t);
        success file new_task_json_list end
    | _ -> success file new_task_json_list

let add_tag_to_task_by_id (file:string) (id:int) (tag:string) : unit =
  if contains_whitespace_or_hyphen tag then raise (InvalidTag(tag))
  else 
    let task_tags = get_task_by_id file id |> Task.tags in
    if List.mem tag task_tags then raise (DuplicateTag(tag))
    else let new_tags = List.cons tag task_tags in 
      update_property_by_id file id (Task.Tags(new_tags))

let remove_tag_from_task_by_id (file:string) (id:int) (tag:string) : unit = 
  let task_tags = get_task_by_id file id |> Task.tags in
  if not(List.mem tag task_tags) then raise (InvalidTag(tag))
  else let new_tags = List.filter (fun t -> not(t = tag)) task_tags in 
    update_property_by_id file id (Task.Tags(new_tags))

let get_variable_id (var:variable) = 
  match var with
  |NextId(s,v) -> s,v

let get_variable (file:json_file) (var:string) : variable = 
  try
    get_vars_t file |> List.map (fun t -> get_variable_from_json_obj t) 
    |> List.filter (fun v -> get_variable_id v |> fst = var)
    |> List.hd
  with Failure(_) -> raise (InvalidVariable(var))

let set_variable (file:json_file) (var:variable) : unit = 
  let new_var_assoc = get_vars_t file |> List.map (fun t -> 
      get_variable_from_json_obj t) |> List.map (fun v -> 
      if fst (get_variable_id v) = fst (get_variable_id var) then
        var else v) |> get_json_obj_list_from_vars in
  write_to_file file new_var_assoc (get_tasks_t file)

(** [increment_next_id_variable file] increments the value bound to the 
    variable with identifier "next_id" in [json_file]. Raises: 
    [InvalidVariable("next_id")] if no such variable exists in [file]. *)
let increment_next_id_variable (file:json_file) : unit =
  let NextId(_,n) = get_variable file "next_id" in
  set_variable file (NextId("next_id",n+1))

let add_task (file:json_file) (task:Task.t) : unit = 
  let task_id = Task.id task in
  let task_desc = Task.description task in
  if task_exists_with_id file task_id then raise (DuplicateId(task_id))
  else if task_exists_with_description file task_desc then
    raise (DuplicateDescription(task_desc))
  else if String.get task_desc 0 = '#' then 
    raise (InvalidDescription(task_desc))
  else
    let new_task_list = [task] |> List.append (get_all_tasks file)
                        |> get_json_obj_list_from_tasks in
    write_to_file file (get_vars_t file) (new_task_list);
    increment_next_id_variable file

(*BISECT-IGNORE-BEGIN*)
type task =  {id:int;
              description:string;
              completed:bool;
              deadline:string;
              tags:string list;
              priority: int}

let tag_helper j = j |> member "tagid" |> to_string

let deadline_helper j = 
  let res = j |> member "deadline" |> member "set" |> to_bool in 
  if res = true 
  then (j |> member "deadline" |> member "value" |> to_float |> readtime) 
  else "None"

(** [task_of_display j] returns a task from the json j in a record format*)
let task_of_display j = {
  id = j |> member "id" |> to_int;
  description = j |> member "description" |> to_string;
  completed = j |> member "completed" |> to_bool;
  deadline = deadline_helper j;
  tags = j |> member "tags" |> to_list |> List.map tag_helper ;
  priority = j|>member "priority"|>to_int;
}

(** [see_all_tasks j] returns all the current tasks in j. 
*)
let see_all_tasks j = 
  j |> member "tasks" |> to_list |> List.map task_of_display

let (lst:task list) = [{id = 1; description = "Do laundry"; completed = false;
                        deadline = "04/19/2019 20:00"; tags = ["life"; "work"];
                        priority = 2};
                       {id = 3; description = "scheduler"; completed = false;
                        deadline = "04/19/2019 20:16"; tags = ["school"];
                        priority = 9}]

(**[convert_task t] converts t from a Task.t to a task*)
let convert_task t ={
  id = id t;
  description = description t;
  completed = completed t;
  deadline = (let dl = deadline t in match dl with 
    |Some x -> readtime(x)
    |None -> "None");
  tags = tags t ;
  priority = priority t;
}

(** [descp_checker str] modifies the description string of a task
    to make it look nice when it is printed all together.*)
let rec descp_checker (str:string) = 
  let len = String.length str in
  if len > 30 then let newstr = String.sub str 0 30 in 
    let index = String.rindex newstr ' ' in 
    String.sub str 0 (index) ^ String.make (30-index) ' ' ^ "\n" ^
    descp_checker (String.sub str (index) (len - (index) )) else 
    str ^ String.make (30-len) ' '

(** [cool_deadline t] modifies the deadline string of a task
    to make it look nice when it is printed all together. *)
let cool_deadline (t:task) = if t.deadline = "None" then "None          None " 
  else
    (let index = String.index t.deadline ' ' 
     in String.sub t.deadline 0 index ^ "    " ^
        String.sub t.deadline (index+1)((String.length t.deadline )-(index+1 )))

(** [initial t] is responsible for making the string format
    Task [#number]: *)
let initial (t:task) = "Task " ^ "[#" ^ (string_of_int (t.id) ) ^ "]:"

(** [check t] adds additional space to a string depending on its length.*)
let check t = let len  = String.length (initial t) in 
  String.make (32-len) ' ' ^ "|  "

(** [booler t] checks if the completed field is true or false and accordingly
    returns Yes or No string*)
let booler t = if t.completed  = true then "Yes" else "No "

let rec tagger lst = 
  match lst with 
  | [] -> "None"
  | [h1] -> h1
  | h::t -> h ^ ", " ^ tagger t

let prty t = 
  if t.priority = 10 then string_of_int (t.priority) ^ "        | " 
  else string_of_int (t.priority) ^ "         | " 

(** [new_rep_2 t] formats a task to a nice string that can be printed out *)
let new_rep_2 (t:task):string  = 
  (initial t)^ (check t) ^ (cool_deadline t) ^  "     | " ^ (booler t)^ 
  "        | " ^ (prty t) ^  (tagger (t.tags)) ^ "\n "  ^ descp_checker(t.description) 

(** [make_tasks lst] makes a formated string of all the tasks so that 
    they can be printed out prettily. *)
let rec make_tasks (lst:task list):string = match lst with 
  | [] -> ""
  | h::t -> new_rep_2 h ^ "\n\n" ^ (make_tasks t) 

let rec make_tasks_gen (lst: task list) (filter: (task -> bool)) 
    (rep: task ->string): string = match lst with 
  | [] -> ""
  | h::t -> if filter h then rep h ^ "\n\n" ^ (make_tasks_gen t filter rep)
    else make_tasks_gen t filter rep 


(** [new_str_oflst j] creates a string for the header of the tasks
    that can nicely be printed out. *)
let new_str_oflst j =
  "\nDescription:" ^ String.make (20) ' ' ^ "   " ^ "Due Date:" ^  "     " ^
  "Due Time:   "^ "Completed:   " ^ "Priority:   " ^ "Category:\n\n" ^ 
  (make_tasks (see_all_tasks j))

(** [new_rep_3 t] formats a task to a nice string that can be printed out
    without the completed section *)
let new_rep_3 (t:task):string  = 
  (initial t)^ (check t)^ (cool_deadline t)^ "     | "^(prty t)^(
    tagger (t.tags)) ^"\n "^ descp_checker(t.description) 


(** [fmake_tasks lst] makes a formated string of all the tasks that are 
    still pending so that 
    they can be printed out prettily.*)
(*let rec fmake_tasks (lst:task list):string = match lst with 
  | [] -> ""
  | h::t -> if h.completed = false then new_rep_3 h ^ "\n\n" ^ 
                                        (fmake_tasks t) else fmake_tasks t*)

(** [future_list j] creates a string for the header of the tasks without
    the completed field that can nicely be printed out. *)
let future_list j = 
  let c_time = Unix.time() in
  let futuredue t= 
    not (completed t) && (match deadline t with 
        | Some x -> (x > c_time)
        | None -> true )  in
  let lst = filter_tasks j (futuredue) in 
  if lst = [] then "You have no incomplete tasks upcoming"
  else let rec printer list acc =
         match list with 
         |[]-> acc
         |h::t ->printer t (acc^(new_rep_3(convert_task h))^"\n\n")
    in printer lst ("\nDescription:" ^ String.make 23 ' '^ "Due Date:" ^ "     " 
                    ^"Due Time:   " ^ "Priority:   " ^ "Category:\n\n")

(** [cmake_tasks lst] makes a formated string of all the tasks that are 
    completed so that 
    they can be printed out prettily.*)
let rec cmake_tasks (lst:task list):string = match lst with 
  | [] -> ""
  | h::t -> if h.completed = true then new_rep_3 h ^ "\n\n" ^ (cmake_tasks t) 
    else cmake_tasks t

(** [completed_list j] creates a string for the header of the tasks without
    the completed field that can nicely be printed out. *)
let completed_list j = 
  "\nDescription:" ^ String.make 23 ' '^ "Due Date:" ^  "     " ^"Due Time:   " 
  ^ "Priority:   " ^ "Category:\n\n" ^ (cmake_tasks (see_all_tasks j))

let rec umake_tasks (lst:task list) (id:int) :string = match lst with 
  | [] -> ""
  | h::t -> if h.id = id then new_rep_2 h ^ "\n\n" ^ (umake_tasks t id) 
    else umake_tasks t id

let make_list j id =  
  "\nDescription:" ^ String.make 23 ' '^ "Due Date:" ^  "     " ^"Due Time:   " 
  ^ "Completed:   " ^ "Priority:   " ^ "Category:\n\n" ^ 
  (umake_tasks (see_all_tasks j) id) 

let rec dmake_tasks (lst:task list) (description:string) :string = 
  match lst with 
  | [] -> ""
  | h::t -> if h.description = description 
    then new_rep_2 h ^ "\n\n" ^ (dmake_tasks t description) 
    else dmake_tasks t description


let dmake_list j description = 
  "\nDescription:" ^ String.make 23 ' '^ "Due Date:" ^  "     " ^"Due Time:   " 
  ^ "Completed:   " ^ "Priority:   " ^ "Category:\n\n" ^ 
  (dmake_tasks (see_all_tasks j) description) 


let next_to_do j = 
  let c_time = Unix.time() in
  let valid t= 
    not (completed t) && (match deadline t with 
        | Some x -> (x > c_time)
        | None -> true )  in
  let lst = filter_tasks j (valid) in 
  if lst = [] then "You have no upcoming deadlines"
  else 
    let calculate task =
      let p = float_of_int(priority task) in let t = match deadline task with 
          | Some x -> x
          | None -> 0. in
      (p /. (abs_float(t-.c_time))/.(86400.)) 
      (*Priority/number of days left; higher number is more important*)
    in
    let rec find_next lst curr = 
      match lst with 
      |[]-> curr;
      |h::t -> if calculate h > calculate curr then find_next t h 
        else find_next t curr
    in let next_task =find_next lst (List.hd (get_all_tasks j)) in
    let desc = description next_task in 
    let fst = "The next task you should complete is Task [#"^
              string_of_int(get_task_id_by_description "data_prep.json" desc)
              ^"] "^desc in
    let rec rest list = match list with
      |[]-> ""
      |h::[] -> let desc = (description h) in
        "\n Finally, you should complete Task [#" ^
        string_of_int(get_task_id_by_description "data_prep.json" desc)^"] "
        ^desc
      |h::t -> let x = find_next t h in let desc = description(x) in
        "\n Then you should do Task [#"^
        string_of_int(get_task_id_by_description "data_prep.json" desc)^"] "
        ^desc^(rest (List.filter (fun y -> y<>x) list))
    in fst ^ (rest (List.filter (fun y -> y<>next_task) lst))


let overdue_list j =
  let c_time = Unix.time() in
  let overdue t= 
    not (completed t) && (match deadline t with 
        | Some x -> (x < c_time)
        | None -> false )  in
  let lst = filter_tasks j (overdue) in 
  if lst = [] then "You have no overdue tasks"
  else let rec printer list acc =
         match list with 
         |[]-> acc
         |h::t ->printer t (acc^(new_rep_3(convert_task h))^"\n\n")
    in printer lst ("\nDescription:" ^ String.make 23 ' '^ 
                    "Due Date:" ^  "     " ^"Due Time:   " 
                    ^ "Priority:   " ^ "Category:\n\n")
(*BISECT-IGNORE-END*)


let date_converter str = let newlst = String.split_on_char ' ' str in 
  let datelst = String.split_on_char '/' (List.hd newlst) in 
  List.map (fun s -> int_of_string s) datelst 

let time_converter str = let newlst = String.split_on_char ' ' str in 
  let timelst = String.split_on_char ':' (List.hd (List.rev newlst)) in 
  List.map (fun s -> int_of_string s) timelst 

let date_helper (t:task) = 
  match date_converter (t.deadline) with 
  | h1::h2::h3::[] -> (h1,h2,h3)
  | _ -> (3,2,3)

let time_helper (t:task) = 
  match time_converter (t.deadline) with 
  | h1::h2::[] -> (h1,h2)
  | _ -> (1,2)

let task_time t = make_unix_time (date_helper t) (time_helper t)

let current_time = time ()

let rec colorer lst id = match lst with 
  | h::t -> if h.id = id then 
      (if (h.deadline = "None" && h.completed = true) 
       then [green] else (if h.deadline = "None" 
                          then [white] else 
                            (if h.completed = false 
                             then (if (task_time h) <= current_time then [white] 
                                   else 
                                     (if (task_time h) -.current_time <= 87609.6 
                                      then [red] else [yellow]) ) 
                             else [green])))
    else colorer t id
  | [] -> []

let color j id = colorer (see_all_tasks j) id

let rec d_colorer lst description = match lst with 
  | h::t -> if h.description = description then 
      (if (h.deadline = "None" && h.completed = true) 
       then [green] else (if h.deadline = "None" 
                          then [white] else 
                            (if h.completed = false then 
                               (if (task_time h) <= current_time 
                                then [white] else 
                                  (if (task_time h) -. current_time <=  87609.6 
                                   then [red] else [yellow]) ) else [green] ) )
      )
    else d_colorer t description
  | [] -> []

let d_color j description = d_colorer (see_all_tasks j) description