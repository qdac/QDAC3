unit sbtree;

{$inline on}

interface

type
  pnode = ^node;

  node = record
    data: longint;
    size: longint;
    lch, rch: pnode;
  end;

  TSizeBalancedTree = class(TObject)
  private
    root, null: pnode;
    procedure lrotate(var x: pnode); inline;
    procedure rrotate(var x: pnode); inline;
    procedure maintain(var t: pnode; const flag: boolean); inline;
    procedure TreeInsert(var t: pnode; v: longint); inline;
    function TreeDelete(var t: pnode; v: longint): longint; inline;
    function TreeSelect(var t: pnode; k: longint): longint; inline;
    function TreeFind(var t: pnode; v: longint): boolean; inline;
    function TreeRank(var t: pnode; v: longint): longint; inline;
    function TreeSucc(var t: pnode; v: longint): longint; inline;
    function TreePred(var t: pnode; v: longint): longint; inline;
  public
    constructor Create;
    procedure insert(v: longint);inline;
    function delete(v: longint): longint;inline;
    function select(k: longint): longint;inline;
    function find(v: longint): boolean;inline;
    function rank(v: longint): longint;inline;
    function succ(v: longint): longint;inline;
    function pred(v: longint): longint;inline;
  end;

implementation

constructor TSizeBalancedTree.Create;
begin
  new(null);
  null^.data := 0;
  null^.size := 0;
  null^.lch := null;
  null^.rch := null;
  root := null;
end;

procedure TSizeBalancedTree.lrotate(var x: pnode);
var
  y: pnode;
begin
  y := x^.rch;
  x^.rch := y^.lch;
  y^.lch := x;
  y^.size := x^.size;
  x^.size := x^.lch^.size + x^.rch^.size + 1;
  x := y;
end;

procedure TSizeBalancedTree.rrotate(var x: pnode);
var
  y: pnode;
begin
  y := x^.lch;
  x^.lch := y^.rch;
  y^.rch := x;
  y^.size := x^.size;
  x^.size := x^.lch^.size + x^.rch^.size + 1;
  x := y;
end;

procedure TSizeBalancedTree.maintain(var t: pnode; const flag: boolean);
begin
  if t = null then
    exit;
  if not flag then
    if t^.lch^.lch^.size > t^.rch^.size then
      rrotate(t)
    else if t^.lch^.rch^.size > t^.rch^.size then
    begin
      lrotate(t^.lch);
      rrotate(t);
    end
    else
      exit
  else if t^.rch^.rch^.size > t^.lch^.size then
    lrotate(t)
  else if t^.rch^.lch^.size > t^.lch^.size then
  begin
    rrotate(t^.rch);
    lrotate(t);
  end
  else
    exit;
  maintain(t^.lch, false);
  maintain(t^.rch, true);
  maintain(t, false);
  maintain(t, true);
end;

procedure TSizeBalancedTree.TreeInsert(var t: pnode; v: longint);
begin
  if t = null then
  begin
    new(t);
    t^.data := v;
    t^.size := 1;
    t^.lch := null;
    t^.rch := null;
  end
  else
  begin
    inc(t^.size);
    if v < t^.data then
      TreeInsert(t^.lch, v)
    else
      TreeInsert(t^.rch, v);
    maintain(t, v >= t^.data);
  end;
end;

function TSizeBalancedTree.TreeDelete(var t: pnode; v: longint): longint;
var
  tmp: pnode;
begin
  if t = null then
    exit;
  dec(t^.size);
  if (v = t^.data) or ((v < t^.data) and (t^.lch = null)) or
    ((v > t^.data) and (t^.rch = null)) then
  begin
    TreeDelete := t^.data;
    if (t^.lch = null) or (t^.rch = null) then
    begin
      if t^.lch = null then
      begin
        tmp := t;
        t := tmp^.rch;
        dispose(tmp);
      end;
      if t^.rch = null then
      begin
        tmp := t;
        t := tmp^.lch;
        dispose(tmp);
      end;
    end
    else
      t^.data := TreeDelete(t^.lch, t^.data + 1);
  end
  else if v < t^.data then
    TreeDelete := TreeDelete(t^.lch, v)
  else
    TreeDelete := TreeDelete(t^.rch, v);
end;

function TSizeBalancedTree.TreeSelect(var t: pnode; k: longint): longint;
begin
  if k = t^.lch^.size + 1 then
  begin
    TreeSelect := t^.data;
    exit;
  end;
  if k <= t^.lch^.size then
    TreeSelect := TreeSelect(t^.lch, k)
  else
    TreeSelect := TreeSelect(t^.rch, k - 1 - t^.lch^.size);
end;

function TSizeBalancedTree.TreeFind(var t: pnode; v: longint): boolean;
begin
  if t = null then
  begin
    TreeFind := false;
    exit;
  end;
  if v < t^.data then
    TreeFind := TreeFind(t^.lch, v)
  else
    TreeFind := (v = t^.data) or TreeFind(t^.rch, v);
end;

function TSizeBalancedTree.TreeRank(var t: pnode; v: longint): longint;
begin
  if t = null then
  begin
    TreeRank := 1;
    exit;
  end;
  if v < t^.data then
    TreeRank := TreeRank(t^.lch, v)
  else
    TreeRank := t^.lch^.size + 1 + TreeRank(t^.rch, v);
end;

function TSizeBalancedTree.TreeSucc(var t: pnode; v: longint): longint;
var
  tmp: longint;
begin
  if t = null then
  begin
    TreeSucc := v;
    exit;
  end;
  if v >= t^.data then
    TreeSucc := TreeSucc(t^.rch, v)
  else
  begin
    tmp := TreeSucc(t^.lch, v);
    if tmp = v then
      tmp := t^.data;
    TreeSucc := tmp;
  end;
end;

function TSizeBalancedTree.TreePred(var t: pnode; v: longint): longint;
var
  tmp: longint;
begin
  if t = null then
  begin
    TreePred := v;
    exit;
  end;
  if v <= t^.data then
    TreePred := TreePred(t^.lch, v)
  else
  begin
    tmp := TreePred(t^.rch, v);
    if tmp = v then
      tmp := t^.data;
    TreePred := tmp;
  end;
end;

procedure TSizeBalancedTree.insert(v: longint);
begin
  TreeInsert(root, v);
end;

function TSizeBalancedTree.delete(v: longint): longint;
begin
  delete := TreeDelete(root, v);
end;

function TSizeBalancedTree.select(k: longint): longint;
begin
  select := TreeSelect(root, k);
end;

function TSizeBalancedTree.find(v: longint): boolean;
begin
  find := TreeFind(root, v);
end;

function TSizeBalancedTree.rank(v: longint): longint;
begin
  rank := TreeRank(root, v);
end;

function TSizeBalancedTree.succ(v: longint): longint;
begin
  succ := TreeSucc(root, v);
end;

function TSizeBalancedTree.pred(v: longint): longint;
begin
  pred := TreePred(root, v);
end;

end.
