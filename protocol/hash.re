type t('a) = {
  hash: string,
  data: 'a,
};

module SHA256 = {
  open Mirage_crypto.Hash.SHA256;
  let hash = data => {
    let hash = {
      let data = Marshal.to_string(data, []);
      Cstruct.of_string(data) |> feed(empty) |> get |> Cstruct.to_string;
    };
    {hash, data};
  };
  let verify = (~hash as expected_hash, data) => {
    let t = hash(data);
    t.hash == expected_hash ? Ok(t) : Error("Invalid hash");
  };
};
