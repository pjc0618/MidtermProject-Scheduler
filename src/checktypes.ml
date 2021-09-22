module type DataDriverSig = sig
  type variable = NextId of int
  type json_file = string
  exception InvalidId of int
  exception DuplicateId of int
  val get_all_tasks : json_file -> (int * Task.t) list
  val get_task_by_id : json_file -> int -> Task.t
  val task_exists_with_id : json_file -> int -> bool
  val remove_task : json_file -> int -> unit
  val update_property_in_storage : json_file -> int -> Task.updatable_property -> unit
  val get_variable : json_file -> string -> variable
  val set_variable : json_file -> variable -> unit
  val add_task : json_file -> Task.t -> unit
  type task = {
    id : int;
    description: string;
    deadline: string;
    completed: bool;
  }
  val see_all_tasks : Yojson.Basic.json -> task list
  val string_of_tasks : task list -> string
  val string_of_list : string -> string
  val new_str_oflst : Yojson.Basic.json -> string
end

module DataDriverCheck : DataDriverSig = Datadriver

module type ParserSig = sig
  type object_phrase = string list
  type command = MakeTask of float*string | CompleteTask of int | RemoveTask of int | SeeTasks | Quit
  exception Empty
  exception Malformed
  val readtime: float -> string
  val parse : string -> command
end

module ParserCheck : ParserSig = Parser

module type TaskSig = sig
  type updatable_property = 
    | Description of string
    | Deadline of float
    | Completed of bool
  type t
  val id : t -> int
  val description : t -> string
  val deadline : t -> float
  val completed : t -> bool
  val see_all_tasks: unit
  val get_task_from_id: int -> unit
  val update_property : t -> updatable_property -> t
  val make : int -> string -> float -> bool -> t
end

module TaskCheck : TaskSig = Task

module type AuthorsSig = sig
  val hours_worked : int list
end

module AuthorsCheck : AuthorsSig = Authors
