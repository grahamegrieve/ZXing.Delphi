{
 /***************************************************************************

                           TAContour.pas

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Helper unit for drawing contour maps
--------------------------------------------------------------------------------
Usage:
- Setup a Chart with a ListChartSource and a LineSeries for each contour level.

- Currently, the algorithm can only handle gridded data, i.e. all data points
  must lie on a rectangular grid (rectangle lengths may vary),
  empty grid points are not allowed (and not checked). x and y arrays must
  be in ascending order, distinct points only. No checks are performed
  if these conditions are met.

- At runtime, create a TContourFinder.

    var
      FContourFinder: TContourFinder;

    procedure TForm1.FormCreate(ASender:TObject);
    begin
      FContourFinder := TContourFinder.Create(self);
      with FContourFinder do begin
        OnNeedX := @NeedX;
        OnNeedY := @NeedY;
        OnNeedZ := @NeedZ;
      end;
    end;

- Write event handlers for OnNeedX, OnNeedY, and OnNeedZ such that the
  contour finder can access the data.
  Example: If your data are stored in arrays xData[0..nx-1], yData[0..ny-1],
  zData[0..nx-1, 0..ny-1] then the event handlers would be

    procedure TForm1.NeedX(Sender: TObject; AIndex: Integer; var AValue:Double);
    begin
      AValue := xData[AIndex];
    end;

    procedure TForm1.NeedY(Sender: TObject; AIndex: Integer; var AValue:Double);
    // Note: y indices run along the y axis of the chart, not from top to bottom.
    begin
      AValue := yData[AIndex];
    end;

    procedure TForm1.NeedZ(Sender: TObject; AIndexX, AIndexY: Integer; var AValue:Double);
    begin
      AValue := zData[AIndexX, AIndexY];
    end;

- After the data are loaded into these arrays assign the correct number of
  points in x and y direction to the contour finder by calling its Prepare method:

    FContourFinder.Prepare(Length(xData), Length(yData));

- To draw the contour your chart must contain a ListChartSource and a LineSeries
  for each contour level; the series must use the ListChartSource, but it is
  fine to use the default listsource of the LineSeries.

  For example, if you want to draw contour levels for z = -1, 0, and +1 then
  there must be three LineSeries and three ListChartSources:
  for z = -1:  LineSeries1, ListChartSource1 (or LineSeries1.ListSource)
  for z = 0:   LineSeries2, ListchartSource2 (or LineSeries2.ListSource)
  for z = +1:  LineSeries3, ListChartSource3 (or LineSeries3.ListSource)

  For each contour level call
    FContourFinder.TraceContourAtLevel(level, ListChartSource);

  i.e.
    FContourFinder.TraceContourAtLevel(-1, ListChartSource1);
    FContourFinder.TraceContourAtLevel( 0, ListChartSource2);
    FContourFinder.TraceContourAtLevel(+1, ListChartSource3);

  As usual modify the LineSeries properties to change, for example, the color
  of the contour lines.

  If you call TraceContourAtLevel with the additional (optional) parameter
  MarksPosition (cmpNone, cmpFirst, cmpCenter, cmpLast, cmpRandom) each
  contiguous part of each contour curve is labelled by the value of the
  contour level (formatted according to another optional parameter
  MarksFormat). Use the series Marks property to further format the contour
  labels.

Author: Werner Pamler
********************************************************************************
}

unit TAContour;

{$IFDEF FPC}
  {$mode delphi}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Types,
  Generics.Collections,
  Generics.Defaults;

const
  DEF_CONTOUR_MARKS_FORMAT = '%g';

type
  TContourFinder = class;

  TContourNeedsCoordEvent = procedure (Sender: TObject;
    AIndex: Integer; var AValue: Double) of object;

  TContourNeeds2CoordsEvent = procedure (Sender: TObject;
    AIndexX, AIndexY: Integer; var AValue: Double) of object;

  TContourMarksPosition = (cmpNone, cmpFirst, cmpCenter, cmpLast, cmpRandom);

  TPolygonCloseMode = (pcmOpen, pcmCloseAbove);


  { TContourFinder }

  TContourFinder = class(TComponent)
  private
    FCountX, FCountY: Integer;
    FXMin, FXMax: Double;
    FYMin, FYMax: Double;
    FZMin, FZMax: Double;
    FStatusMap: packed array of packed array of byte;
      // 1st index --> x, 2nd index --> y
    FLevel: Double;
    FListSource: TList<TPointF>;
    FContourMarksPosition: TContourMarksPosition;
    FContourMarksFormat: String;
    FPrepared: Boolean;
    FPolygonCloseMode: TPolygonCloseMode;
    FOnNeedX: TContourNeedsCoordEvent;
    FOnNeedY: TContourNeedsCoordEvent;
    FOnNeedZ: TContourNeeds2CoordsEvent;
    function GetStatus(AIndexX, AIndexY: Integer): Byte;
    procedure SetStatus(AIndexX, AIndexY: Integer; AValue: Byte);

  protected
    // Contour finding
    procedure AddToContour(ABelowPt, AAbovePt: TPoint; var xval, yval: Double);
    function FindNeighbor(ABelowPt, AAbovePt: TPoint): Integer;
    procedure InitStatusMap;
    procedure Interpolate(ABelowPt, AAbovePt: TPoint; var xval, yval: Double);
    function IsAtBorder(APoint: TPoint): Boolean;
    function IsBeyondBorder(APoint: TPoint): Boolean;
    procedure NextNeighbor(var ANeighbor: Integer);
    function NextPoint(APoint: TPoint; ANeighbor:Integer): TPoint;
    function OppositeNeighbor(ANeighbor: Integer): Integer;
    procedure Search(APoint: TPoint; ADirection: Integer);

    // data management
    procedure Error(const AMsg: String); virtual;
    procedure FindDataRange;
    function IsNaNItem(AIndex: Integer): Boolean;
    function NeedX(AIndex: Integer): Double; virtual;
    function NeedY(AIndex: Integer): Double; virtual;
    function NeedZ(AIndexX, AIndexY: Integer): Double; virtual;
    property StatusMap[AIndexX, AIndexY: Integer]: byte read GetStatus write SetStatus;

  public
    procedure Prepare(ACountX, ACountY: Integer);
    procedure TraceContourAtLevel(ALevel: Double; AListSource: TList<TPointF>;
      AMarksPosition: TContourMarksPosition = cmpNone; AMarksFormat: String = '');

    property x[AIndexX: Integer]: Double read NeedX;
    property y[AIndexY: Integer]: Double read NeedY;
    property z[AIndexX, AIndexY: Integer]: Double read NeedZ;
    property Level: Double read FLevel;

  published
    property PolygonCloseMode: TPolygonCloseMode read FPolygonCloseMode write FPolygonCloseMode;
    property OnNeedX: TContourNeedsCoordEvent read FOnNeedX write FOnNeedX;
    property OnNeedY: TContourNeedsCoordEvent read FOnNeedY write FOnNeedY;
    property OnNeedZ: TContourNeeds2CoordsEvent read FOnNeedZ write FOnNeedZ;
  end;

  EContourFinder = class(Exception);


implementation

uses
  Math;

resourcestring
  SContourFinder_NoNeedDataHandler = 'Event handler for %s missing.';
  SContourFinder_NotPrepared = '"Prepare" not called.';

const
  BELOW = 0;
  ABOVE = 1;
  USED = 2;

  // Moore neighbors
  EAST = 0;          // directions of neighbors in clockwise (!) order
  SOUTH_EAST = 1;
  SOUTH = 2;
  SOUTH_WEST = 3;
  WEST = 4;
  NORTH_WEST = 5;
  NORTH = 6;
  NORTH_EAST = 7;

  NEIGHBORS: Array[EAST..NORTH_EAST] of TPoint = (
    (x:+1; y: 0),
    (x:+1; y:-1),
    (x: 0; y:-1),
    (x:-1; y:-1),
    (x:-1; y: 0),
    (x:-1; y:+1),
    (x: 0; y:+1),
    (x:+1; y:+1)
  );


{ TContourFinder }

{ Add the interpolated point where the connection between ABelowPt and AAbovePt
  crosses the contour level to the ListSource and marks the AbovePt as USED.
  If the BelowPt is in the border area surrounding the data array NaN values
  are put into the ListSource because no contour level can be drawn outside
  the data area. }
procedure TContourFinder.AddToContour(ABelowPt, AAbovePt: TPoint;
  var xval, yval: Double);
var
  aValue:TPointF;
begin
  if IsBeyondBorder(ABelowPt) then begin
    case PolygonCloseMode of
      pcmOpen:
        begin
          xval := NaN;
          yval := NaN;
        end;
      pcmCloseAbove:  // Polygon closes for parts above section plane
        begin
          if ABelowPt.X = -1 then begin
            xval := x[0];
            yval := y[ABelowPt.y];
          end else
          if ABelowPt.X = FCountX then begin
            xval := x[FCountX-1];
            yval := y[ABelowPt.Y];
          end else
          if ABelowPt.Y = -1 then begin
            xval := x[ABelowPt.X];
            yval := y[0];
          end else
          if ABelowPt.Y = FCountY then begin
            xval := x[ABelowPt.X];
            yval := y[FCountY-1];
          end;
        end;
    end;
  end else
    Interpolate(ABelowPt, AAbovePt, xval, yval);
  aValue.x:=xval;
  aValue.y:=yval;
  with FListSource do
    Add(aValue);
    // for debugging only
    //Add(xval, yval, Format('(%d/%d) -> (%d/%d)', [ABelowPt.X, ABelowPt.Y, AAbovePt.X, AAbovePt.Y]));
  StatusMap[AAbovePt.x, AAbovePt.y] := USED;
end;


{ Raises an exception in case of an error. }
procedure TContourFinder.Error(const AMsg: String);
begin
  raise EContourFinder.Create(AMsg);
end;


{ Determines the minimum and maximum value of xData, yData, and zData. }
procedure TContourFinder.FindDataRange;
var
  ix, iy : Integer;
  value: Double;
begin
  // Since the data must be ordered we simply get the minimum and maximum
  // x and y values from the first and last data values.
  FXMin := x[0];
  FXMax := x[FCountX-1];
  FYMin := y[0];
  FYMax := y[FCountY-1];
  FZMin := z[0, 0];
  FZMax := z[0, 0];
  for ix := 0 to FCountX - 1 do
    for iy := 0 to FCountY - 1 do begin
      value := z[ix, iy];
      FZMin := Min(FZMin, value);
      FZMax := Max(FZMax, value);
    end;
end;


{ Finds the neighborhood index for going from AAbovePt to ABelowPt }
function TContourFinder.FindNeighbor(ABelowPt, AAbovePt: TPoint): Integer;
var
  dx, dy: Integer;
begin
  dx := ABelowPt.X - AAbovePt.X;
  dy := ABelowPt.Y - AAbovePt.Y;
  Assert((abs(dx) <= 1) and (abs(dy) <= 1));
  for Result := 0 to 7 do
    if (dx = NEIGHBORS[Result].X) and (dy = NEIGHBORS[Result].Y) then
      exit;
end;


{ Returns the value of the status array. Takes care of the border region around
  the data area. }
function TContourFinder.GetStatus(AIndexX, AIndexY: Integer): Byte;
begin
  result := FStatusMap[AIndexX+1, AIndexY+1];
end;


{ Initialize the status array. }
procedure TContourFinder.InitStatusMap;
var
  ix, iy: Integer;
begin
  SetLength(FStatusMap, FCountX + 2, FCountY + 2);
  // +2 because also regions outside the grid are needed

  for ix := -1 to FCountX do
    for iy := -1 to FCountY do
      if IsBeyondBorder(Point(ix, iy)) then
        StatusMap[ix, iy] := BELOW     // beyond the data region: always "below"
      else
      if z[ix, iy] < FLevel then
        StatusMap[ix, iy] := BELOW
      else
        FStatusMap[ix+1, iy+1] := ABOVE;
  // Note: Indices in FStatusMap are off by 1 compared to the data arrays!
  // But the property StatusMap takes care of that.
end;


{ The connection between given "below" and "above" points intersects the
  contour level. Performs a linear interpolation to find the x,y coordinates
  with the contour level is intersected. }
procedure TContourFinder.Interpolate(ABelowPt, AAbovePt: TPoint;
  var xval, yval: Double);
var
  x1,x2, y1,y2, z1,z2: Double;
begin
  if ABelowPt.X < 0 then
    xval := FXMin
  else if ABelowPt.X >= FCountX then
    xval := FXMax
  else begin
    x1 := x[ABelowPt.x];
    x2 := x[AAbovePt.x];
    z1 := z[ABelowPt.x, ABelowPt.y];
    z2 := z[AAbovePt.x, AAbovePt.y];
    xval := x1 + (FLevel - z1) / (z2 - z1) * (x2 - x1);
  end;

  if ABelowPt.Y < 0 then
    yval := FYMin
  else if ABelowPt.Y >= FCountY then
    yval := FYMax
  else begin
    y1 := y[ABelowPt.y];
    y2 := y[AAbovePt.y];
    z1 := z[ABelowPt.x, ABelowPt.y];
    z2 := z[AAbovePt.x, AAbovePt.y];
    yval := y1 + (FLevel - z1) / (z2 - z1) * (y2 - y1);
  end;
end;


{ Checks whether a point is at the border of the data area. }
function TContourFinder.IsAtBorder(APoint: TPoint): Boolean;
begin
  result := (APoint.X = 0) or (APoint.Y = 0) or
    (APoint.X = FCountX - 1) or (APoint.Y = FCountY - 1);
end;


{ Checks whether a point is outside the data area }
function TContourFinder.IsBeyondBorder(APoint: TPoint): Boolean;
begin
  result := (APoint.X = -1) or (APoint.Y = -1) or
    (APoint.X = FCountX) or (APoint.Y = FCountY);
end;


function TContourFinder.IsNaNitem(AIndex: Integer): Boolean;
var
  item: TPointF;
begin
  item := FListSource.Items[AIndex];
  Result := IsNaN(item.X) or IsNaN(item.Y);
end;


{ Queries the x coordinate of the data point with the given x index }
function TContourFinder.NeedX(AIndex: Integer): Double;
begin
  Result:=AIndex;
end;


{ Queries the y coordinate of the data point with the given y index }
function TContourFinder.NeedY(AIndex: Integer): Double;
begin
  Result:=AIndex;
end;


{ Queries the z coordinate of the data point with the given x and y indices }
function TContourFinder.NeedZ(AIndexX, AIndexY: Integer): Double;
begin
  FOnNeedZ(Self, AIndexX, AIndexY, Result)
end;


{ Finds the next neighbor in the Moore neighborhood in clockwise direction }
procedure TContourFinder.NextNeighbor(var ANeighbor: Integer);
begin
  ANeighbor := (ANeighbor + 1) mod 8;
end;


{ Finds the next point in the given direction, starting a APoint }
function TContourFinder.NextPoint(APoint: TPoint; ANeighbor:Integer): TPoint;
begin
  Result.X := APoint.X + NEIGHBORS[ANeighbor].x;
  Result.Y := APoint.Y + NEIGHBORS[ANeighbor].y;
end;


{ Finds the opposite direction of a given direction }
function TContourFinder.OppositeNeighbor(ANeighbor: Integer): Integer;
begin
  Result := (ANeighbor + 4) mod 8;
end;


{ Prepares the contour finder by giving it the size of the data array.
  Must be called before "TraceContourLevelAt". }
procedure TContourFinder.Prepare(ACountX, ACountY: Integer);
begin
  FCountX := ACountX;
  FCountY := ACountY;
  if not Assigned(FOnNeedZ) then
    Error(Format(SContourFinder_NoNeedDataHandler, ['OnNeedZ']));
  FindDataRange;
  FPrepared := true;
end;


{ Traces the contour using the "Moore-Neighborhood Algorithm" (see:
  http://en.wikipedia.org/wiki/Moore_neighborhood or
  http://www.imageprocessingplace.com/downloads_V3/root_downloads/tutorials/contour_tracing_Abeer_George_Ghuneim/moore.html)
  APoint was found by TraceContourAtLevel to be below the contour level,
  its neighbor in the given direction is above the contour level.
  In the Moore-Neighborhood algorithm all neighbors of an "above" point are
  scanned in clockwise direction for another point which is above contour level.
  Then the neighborhood of this point is scanned in the same way until the
  starting point is reached again.
  Special treatment is necessary for points at the edge of the data area. For
  this purpose the data area is surrounded by a rim of "below" points. If an
  "above" point is found from this rim, NaNs are added because we do not want
  to draw contour lines there. }
procedure TContourFinder.Search(APoint: TPoint; ADirection: integer);
var
  origBelowPt: TPoint;
  origAbovePt: TPoint;
  counter: Integer;
  belowPt, abovePt, testPt: TPoint;
  neighbor: Integer;
  prevneighbor: Integer;
  firstX, firstY: Double;
  xval, yval: Double;
  aValue:TPointF;
  function IsClosed: Boolean;
  begin
    Result := false;
    if (belowPt.X = origBelowPt.x) and (belowPt.Y = origBelowPt.Y) and
       (abovePt.X = origAbovePt.x) and (abovePt.Y = origAbovePt.Y)
    then begin
      if not IsNaN(firstX) and not IsNaN(firstY) and
         not IsNaN(xval) and not IsNaN(yval) and
        ((xval <> firstX) or (yval <> firstY))
      then
      begin
        aValue.x:=firstX;
        aValue.y:=firstY;
        FListSource.Add(aValue);
      end;
      Result := true;
    end;
  end;

begin
  counter := 0;
  belowPt := APoint;
  abovePt := NextPoint(belowPt, ADirection);
  origBelowPt := belowPt;
  origAbovePt := abovePt;
  if FListSource.Count > 0 then
  begin
    aValue.x:=NaN;
    aValue.y:=NaN;
    FListSource.Add(aValue);
  end;
  AddToContour(belowPt, abovePt, firstX, firstY);
  neighbor := OppositeNeighbor(ADirection);
  NextNeighbor(neighbor);
  repeat
    testPt := NextPoint(abovePt, neighbor);
    case StatusMap[testPt.X, testPt.Y] of
      ABOVE, USED:
        begin
          abovePt := testPt;
          AddToContour(belowPt, abovePt, xval, yval);
          neighbor := FindNeighbor(belowPt, abovePt);
          counter := 0;
          if IsClosed then
            exit;
        end;
      BELOW:
        begin
          belowpt := testpt;
          if IsAtBorder(belowPt) then
            AddToContour(belowPt, abovePt, xval, yval);
          if IsClosed then
            exit;
          NextNeighbor(neighbor);
          inc(counter);
          if counter = 8 then
            exit;
        end;
    end;
  until false;
end;


{ Sets a status value (BELOW, ABOVE, or USED). The indices take care of the
  fact that the status array reserves a rim around the data region. }
procedure TContourFinder.SetStatus(AIndexX, AIndexY: Integer; AValue: Byte);
begin
  FStatusMap[AIndexX+1, AIndexY+1] := AValue;
end;


{ Main contour tracing method. Scans the data points by columns until a pair
  of points is found where the given contour level is crossed. From that location
  the contour is traced by means of the Moore Neighborhood Algorithm.
  Note: Is is required to call Prepare before TraceContourAtLevel. }
procedure TContourFinder.TraceContourAtLevel(ALevel: Double;
  AListSource: TList<TPointF>; AMarksPosition: TContourMarksPosition;
  AMarksFormat: String);
var
  ix, iy: Integer;
begin
  Assert(AListSource <> nil);
  if not FPrepared then
    Error(SContourFinder_NotPrepared);

  FLevel := ALevel;
  FListSource := AListSource;
  FContourMarksPosition := AMarksPosition;
  if AMarksFormat = '' then
    FContourMarksFormat := DEF_CONTOUR_MARKS_FORMAT
  else
    FContourMarksFormat := AMarksFormat;

  FListSource.Clear;
  InitStatusMap;
  for ix := 0 to FCountX-1 do
    for iy := -1 to FCountY-1 do
      if (StatusMap[ix, iy] = BELOW) and (StatusMap[ix, iy+1] = ABOVE) then
        Search(Point(ix, iy), NORTH);
end;


end.

