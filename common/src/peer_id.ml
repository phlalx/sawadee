
include Hash_id.Id

let client s = Core.String.prefix s 7

let to_string_hum s = (to_string_hum s) ^ (client s)