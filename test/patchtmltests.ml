let () =
  let test_cases = [|
    (".", "index.html");
    ("./", "index.html");
    ("..", "../../index.html");
    ("..a", "../..a/index.html");
    ("./example/", "../example/index.html");
    ("example/custom/", "../example/custom/index.html");
    ("http://www.github.com/", "http://www.github.com/");
    ("https://www.github.com/", "https://www.github.com/");
    ("./example/custom//", "../example/custom/index.html");
    ("../example/custom/", "../../example/custom/index.html");
    ("../../dir1/dir2/../dir3/dir4/", "../../../dir1/dir2/../dir3/dir4/index.html");
    ("./././/.///././././/.//./././.././.../././.././././././/./", "../../.../../index.html");
    ("hello/ooo/\\", "../hello/ooo/\\/index.html");
  |]
  in

  Array.iter (fun (sent, expected)->
    let open Omd  in
    let sent = {
      label =  Text ([("plop","salut")], "cc");
      destination = sent;
      title = None
    }
    in
    let expected = {
      label = Text ([("plop","salut")], "cc");
      destination = expected;
      title = None
    }
    in
    let result = Htmlize.Patchtml.handle_link ~is_raw:false sent in
    let ok = result = expected in
    if not ok then Format.printf "expected: %s but got %s@." expected.destination sent.destination;

    assert ok
  ) test_cases;

  let img_test_cases = [|
    (".", "raw/.");
    ("./", "raw/.");
    ("../.", "../../raw/.");
    ("./image.png", "../image.png/raw/image.png");
    ("../image.png", "../../image.png/raw/image.png");
    ("..////image.png", "../../image.png/raw/image.png");
    ("..////.../image.png", "../../.../image.png/raw/image.png");
    ("../dir/../dir/image.png", "../../dir/../dir/image.png/raw/image.png");
  |]
  in

  Array.iter (fun (sent, expected)->
    let open Omd  in
    let sent = {
      label =  Text ([("plop","salut")], "cc");
      destination = sent;
      title = None
    }
    in
    let expected = {
      label = Text ([("plop","salut")], "cc");
      destination = expected;
      title = None
    }
    in
    let result = Htmlize.Patchtml.handle_link ~is_raw:true sent in
    let ok = result = expected in
    if not ok then Format.printf "expected: %s but got %s@." expected.destination sent.destination;

    assert ok
  ) img_test_cases
