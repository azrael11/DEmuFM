unit timer_engine;

interface

uses
  Fmx.Dialogs;

const
  MAX_TIMERS = 14;

type
  exec_type_param = procedure(param0: byte);
  exec_type_simple = procedure;

  ttimers = record
    execute_param: exec_type_param; // call function
    execute_simple: exec_type_simple; // sound call function
    time_final: single; // Final time to call function
    actual_time: single; // Actual time
    cpu: byte; // CPU asociada al timer y dispositivo asociado
    enabled: boolean; // Running?
    param0: byte; // Parametros
  end;

  tautofire_proc = procedure(autofire_index: byte; autofire_status: boolean);

  timer_eng = class
    constructor create;
    destructor free;
  public
    timer: array [0 .. MAX_TIMERS] of ttimers;
    autofire_timer: byte;
    autofire_on: boolean;
    autofire_status, autofire_enabled: array [0 .. 11] of boolean;
    function init(cpu: byte; time: single; exec_simple: exec_type_simple;
      exec_param: exec_type_param; ena: boolean; param0: byte = 0): byte;
    procedure update(time_add: word; cpu: byte);
    procedure enabled(timer_num: byte; state: boolean);
    procedure reset(timer_num: byte);
    procedure clear;
    procedure autofire_init;
  private
    timer_count: integer;
  end;

var
  timers: timer_eng;

procedure one_shot_timer_0(cpu:byte;time:single;final_procedure:exec_type_simple);
procedure one_shot_timer_1(cpu:byte;time:single;final_procedure:exec_type_simple);

implementation
uses controls_engine,cpu_misc;

var
  one_shot_0,one_shot_1:ttimers;

procedure one_shot_timer_0(cpu:byte;time:single;final_procedure:exec_type_simple);
begin
  one_shot_0.cpu:=cpu;
  one_shot_0.actual_time:=0;
  one_shot_0.time_final:=time;
  one_shot_0.enabled:=true;
  one_shot_0.execute_simple:=final_procedure;
end;

procedure one_shot_timer_1(cpu:byte;time:single;final_procedure:exec_type_simple);
begin
  one_shot_1.cpu:=cpu;
  one_shot_1.actual_time:=0;
  one_shot_1.time_final:=time;
  one_shot_1.enabled:=true;
  one_shot_1.execute_simple:=final_procedure;
end;

procedure auto_fire;
begin
  // P1
  if timers.autofire_enabled[0] then
  begin
    if timers.autofire_status[0] then
      p_contrls.map_arcade.but0[0] := not(p_contrls.map_arcade.but0[0])
    else
      p_contrls.map_arcade.but0[0] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[1] then
  begin
    if timers.autofire_status[1] then
      p_contrls.map_arcade.but1[0] := not(p_contrls.map_arcade.but1[0])
    else
      p_contrls.map_arcade.but1[0] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[2] then
  begin
    if timers.autofire_status[2] then
      p_contrls.map_arcade.but2[0] := not(p_contrls.map_arcade.but2[0])
    else
      p_contrls.map_arcade.but2[0] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[3] then
  begin
    if timers.autofire_status[3] then
      p_contrls.map_arcade.but3[0] := not(p_contrls.map_arcade.but3[0])
    else
      p_contrls.map_arcade.but3[0] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[4] then
  begin
    if timers.autofire_status[4] then
      p_contrls.map_arcade.but4[0] := not(p_contrls.map_arcade.but4[0])
    else
      p_contrls.map_arcade.but4[0] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[5] then
  begin
    if timers.autofire_status[5] then
      p_contrls.map_arcade.but5[0] := not(p_contrls.map_arcade.but5[0])
    else
      p_contrls.map_arcade.but5[0] := false;
    event.arcade := true;
  end;
  // P2
  if timers.autofire_enabled[6] then
  begin
    if timers.autofire_status[6] then
      p_contrls.map_arcade.but0[1] := not(p_contrls.map_arcade.but0[1])
    else
      p_contrls.map_arcade.but0[1] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[7] then
  begin
    if timers.autofire_status[7] then
      p_contrls.map_arcade.but1[1] := not(p_contrls.map_arcade.but1[1])
    else
      p_contrls.map_arcade.but1[1] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[8] then
  begin
    if timers.autofire_status[8] then
      p_contrls.map_arcade.but2[1] := not(p_contrls.map_arcade.but2[1])
    else
      p_contrls.map_arcade.but2[1] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[9] then
  begin
    if timers.autofire_status[9] then
      p_contrls.map_arcade.but3[1] := not(p_contrls.map_arcade.but3[1])
    else
      p_contrls.map_arcade.but3[1] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[10] then
  begin
    if timers.autofire_status[10] then
      p_contrls.map_arcade.but4[1] := not(p_contrls.map_arcade.but4[1])
    else
      p_contrls.map_arcade.but4[1] := false;
    event.arcade := true;
  end;
  if timers.autofire_enabled[11] then
  begin
    if timers.autofire_status[11] then
      p_contrls.map_arcade.but5[1] := not(p_contrls.map_arcade.but5[1])
    else
      p_contrls.map_arcade.but5[1] := false;
    event.arcade := true;
  end;
end;

constructor timer_eng.create;
begin
  self.clear;
end;

destructor timer_eng.free;
begin
end;

procedure timer_eng.autofire_init;
begin
  //Siempre contra la primera CPU!!!
  self.autofire_timer:=self.init(0,1,auto_fire,nil,timers.autofire_on);
  self.timer[self.autofire_timer].time_final:=350;//cpu_0_clock/10000;
end;

function timer_eng.init(cpu: byte; time: single; exec_simple: exec_type_simple;
  exec_param: exec_type_param; ena: boolean; param0: byte = 0): byte;
begin
  self.timer_count := self.timer_count + 1;
  // if self.timer_count = MAX_TIMERS then
  // MessageDlg('Superados el maximo de timer', mtInformation, [mbOk], 0);
  self.timer[self.timer_count].cpu := cpu;
  self.timer[self.timer_count].time_final := time;
  self.timer[self.timer_count].execute_param := exec_param;
  self.timer[self.timer_count].execute_simple := exec_simple;
  self.timer[self.timer_count].enabled := ena;
  self.timer[self.timer_count].param0 := param0;
  init := self.timer_count;
end;

procedure timer_eng.update(time_add:word;cpu:byte);
var
  f:integer;
begin
for f:=self.timer_count downto 0 do begin
  if (self.timer[f].enabled and (cpu=self.timer[f].cpu)) then begin
    self.timer[f].actual_time:=self.timer[f].actual_time+time_add;
    //Atencion!!! si desactivo el timer dentro de la funcion, ya no tiene que hacer nada!
    while ((self.timer[f].actual_time>=self.timer[f].time_final) and self.timer[f].enabled) do begin
        self.timer[f].actual_time:=self.timer[f].actual_time-self.timer[f].time_final;
        if @self.timer[f].execute_simple<>nil then self.timer[f].execute_simple
          else self.timer[f].execute_param(self.timer[f].param0);
    end;
  end;
end;
if ((one_shot_0.enabled) and (one_shot_0.cpu=cpu)) then begin
  one_shot_0.actual_time:=one_shot_0.actual_time+time_add;
  if one_shot_0.actual_time>=one_shot_0.time_final then begin
    one_shot_0.enabled:=false;
    one_shot_0.execute_simple;
  end;
end;
if ((one_shot_1.enabled) and (one_shot_1.cpu=cpu)) then begin
  one_shot_1.actual_time:=one_shot_1.actual_time+time_add;
  if one_shot_1.actual_time>=one_shot_1.time_final then begin
    one_shot_1.enabled:=false;
    one_shot_1.execute_simple;
  end;
end;
end;

procedure timer_eng.enabled(timer_num:byte;state:boolean);
begin
  //Esto le sienta mal a Jackal!!!
  //if (state and not(self.timer[timer_num].enabled)) then self.timer[timer_num].actual_time:=0;
  self.timer[timer_num].enabled:=state;
end;

procedure timer_eng.clear;
var
  f:byte;
begin
self.timer_count:=-1;
for f:=0 to MAX_TIMERS do begin
  self.timer[f].time_final:=0;
  self.timer[f].actual_time:=0;
  self.timer[f].execute_param:=nil;
  self.timer[f].execute_simple:=nil;
  self.timer[f].cpu:=0;
  self.timer[f].enabled:=false;
end;
for f:=0 to 11 do autofire_status[f]:=false;
one_shot_0.execute_simple:=nil;
one_shot_1.execute_simple:=nil;
end;

procedure timer_eng.reset(timer_num: byte);
begin
  self.timer[timer_num].actual_time:=0;
  one_shot_0.actual_time:=0;
  one_shot_1.actual_time:=0;
end;

end.
