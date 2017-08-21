
include Hash_id.Id

let sha1_of_string s = Sha1.string s |> Sha1.to_bin 