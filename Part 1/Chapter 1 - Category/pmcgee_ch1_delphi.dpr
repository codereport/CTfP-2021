program Chapter1;
{$APPTYPE CONSOLE}

uses
  System.SysUtils, generics.Defaults, Generics.Collections;

type
  Tfnc = class
    public
      class function          Id<A>     ( x : A)          : A;
      class function     Compose<A,B,C> ( f : TFunc<A,B>;
                                          g : TFunc<B,C>) : TFunc<A,C>;

      class function          Eq<A>     (x,y: A)          : boolean;
      class function  TestLeftId<A>     ( f : TFunc<A,A>) : boolean;
      class function TestRightId<A>     ( f : TFunc<A,A>) : boolean;
  end;


//  --  --  --  --  --  --


  class function Tfnc.Id<A>(x:A): A;
  begin
    result := x;
  end;


  class function Tfnc.Compose<A,B,C> ( f : TFunc<A,B>;
                                       g : TFunc<B,C> ): TFunc<A,C>;
  begin
     result := function(a:A): C
                  begin  result := g(f(a));  end;
  end;


  class function Tfnc.eq<A>(x,y:A) : boolean;
  begin
     var
       comp := TComparer<A>.Default;
     result := comp.compare(x,y)= 0;
  end;


  class function Tfnc. TestLeftId<A>  (f:TFunc<A,A>) : boolean;
  begin
     result := eq<A>( Id<A>(f(default(A))) , f(default(A)) );
  end;


  class function Tfnc.TestRightId<A> (f:TFunc<A,A>) : boolean;
  begin
     result := eq<A>( f(Id<A>(default(A))) , f(default(A)) );
  end;


//  --  --  --  --  --  --

  function plusone(i:integer):integer;
  begin
    result := i + 1;
  end;

  function plusstr(s:string):string;
  begin
    result := '*' + s + '*';
  end;

//  --  --  --  --  --  --

begin
  writeln;
  if Tfnc.TestLeftId <integer>(plusone) then  writeln('Left  Id<integer> compose : tick');
  if Tfnc.TestRightId<integer>(plusone) then  writeln('Right Id<integer> compose : tick');

  writeln;
  if Tfnc.TestLeftId <string >(plusstr) then  writeln('Left  Id<string > compose : tick');
  if Tfnc.TestRightId<string >(plusstr) then  writeln('Right Id<string > compose : tick');

  writeln;
  readln;
end.
