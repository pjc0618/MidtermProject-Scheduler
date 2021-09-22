(** A type representing a variable that can be read or updated from storage. *)
type variable = 
  | NextId of string * int

(** A type synonym for the data file's relative path. *)
type json_file = string

exception InvalidId of int
exception DuplicateId of int
exception DuplicateDescription of string
exception DuplicateTag of string
exception InvalidDescription of string
exception InvalidTag of string
exception InvalidVariable of string
exception InvalidJsonFormat of string

(** [get_all_tasks file] is a list of all tasks in [file]. If there are no
    tasks in [file], this returns [[]]. *)
val get_all_tasks : json_file -> Task.t list

(** [filter_tasks file p] is a list of all tasks in [file] satsifying predicate
    [p]. If no tasks in [file] satisfy [p], this returns [[]]. *)
val filter_tasks : json_file -> (Task.t -> bool) -> Task.t list

(** [get_task_by_id file id] is the [Task.t] with id [id] in [file].
    Raises: [InvalidId(id)] when no task with such id exists in storage. *)
val get_task_by_id : json_file -> int -> Task.t

(** [task_exists_with_id file id] is whether there exists a task with id 
    [id] in [file]. *)
val task_exists_with_id : json_file -> int -> bool

(** [get_tasks_by_tag file tag] is a [Task.t] list containing all tasks in
    [file] with tag [tag] in their tag list. If no task exists with the given 
    tag in [file], this returns [[]]. *)
val get_tasks_by_tag : json_file -> string -> Task.t list

(** [get_task_by_description file desc] is the task id of the task with unique
    description [desc]. Raises: [InvalidDescription(desc)] if no task exists
    in [file] with description [desc]. *)
val get_task_id_by_description : json_file -> string -> int

(** [task_exists_with_description file desc] is whether a task in [file] exists
    with a description [desc]. *)
val task_exists_with_description : json_file -> string -> bool

(** [remove_task_by_id file id] removes the task with id [id] from [file].
    Raises: [InvalidId(id)] when no task with such id exists in storage. Raises:
    [InvalidJsonFormat] when the supplied file is not in the correct format. *)
val remove_task_by_id : json_file -> int -> unit

(** [update_property_by_id file id property] updates the task with id [id]
    and sets its property [property] to the value bound to the variant
    [property]. Raises: [InvalidId(id)] when no task with such id exists in
    [file]. *)
val update_property_by_id :
  json_file -> int -> Task.updatable_property -> unit

(** [add_tag_to_task_by_id file id tag] adds tag [tag] to the set of tags bound
    to task with id [id] in storage. Raises: [InvalidId(id)] when no task exists
    in [file] with id [id]. Raises: [InvalidTag(tag)] when [tag] contains a
    whitespace or hyphen character. Raises: [DuplicateTag(tag)] when task with
    id [id] already has tag [tag].*)
val add_tag_to_task_by_id : json_file -> int -> string -> unit

(** [remove_tag_from_task_by_id file id tag] removes tag [tag] from the set of
    tags bound to task with id [id] in storage. Raises: [InvalidId(id)] when no
    task exists in [file] with id [id]. Raises: [InvalidTag(tag)] when the task
    with id [id] does not have a tag named [tag]. *)
val remove_tag_from_task_by_id : json_file -> int -> string -> unit

(** [get_variable_id var] is the identifier bound to [var]. *)
val get_variable_id : variable -> string * int

(** [get_variable file var] is a [variable] that stores the value of the
    variable named [var] in [file]. Raises: [InvalidVariable] when [var] does
    not correspond to a valid variable identifier in storage. Raises: 
    [InvalidJsonFormat] when the supplied file is not in the correct format. *)
val get_variable : json_file -> string -> variable

(** [set_variable file var] sets the variable [var] to its underlying value in
    [file]. *)
val set_variable : json_file -> variable -> unit

(** [add_task file task] adds [task] to [file]. 
    Raises: [DuplicateId (Task.id task)] when a task with id [Task.id task] 
    already exists in storage. Raises:
    [DuplicateDescription (Task.description task)] when a task already exists in
    [file] with the description bound to [task]. Raises:
    [InvalidDescription (Task.description task)] when the supplied task has a
    description that begins with the # character. Raises: [InvalidJsonFormat]
    when the supplied file is not in the correct format. *)
val add_task : json_file -> Task.t -> unit

type task =  {id:int;
              description:string;
              completed:bool;
              deadline:string;
              tags:string list;
              priority:int}

(** [see_all_tasks j] returns all the current tasks in j. 
*)
val see_all_tasks : Yojson.Basic.json -> task list

(** [new_str_oflst j] creates a string for the header of the tasks
    that can nicely be printed out. *)
val new_str_oflst : Yojson.Basic.json -> string

(** [future_list j] creates a string for the header of the tasks with a due date
    in the future that can nicely be printed out. *)
val future_list : json_file -> string

(** [completed_list j] creates a string for the header of the tasks without
    the completed field that can nicely be printed out. *)
val completed_list : Yojson.Basic.json -> string

val make_list : Yojson.Basic.json -> int -> string

val dmake_list: Yojson.Basic.json -> string -> string

val color: Yojson.Basic.json -> int ->  ANSITerminal.style list

val d_color: Yojson.Basic.json -> string ->  ANSITerminal.style list

(** [next_task j] returns the task that should be completed next based on the 
    priorities of the given tasks and the time remaining to do them*)
val next_to_do: json_file -> string

(** [overdue_list j] creates a string for the header of the tasks that are past
    due that can nicely be printed out. *)
val overdue_list : json_file -> string
