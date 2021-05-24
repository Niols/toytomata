type reason = [
  | `Timeout
  | `SizeLimit
  | `Undecidable
]

exception DontKnow of reason

type t = [
  | `True
  | `False
  | `DontKnow of reason
]
