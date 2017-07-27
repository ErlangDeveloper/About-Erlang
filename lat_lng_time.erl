%%%-------------------------------------------------------------------
%%% @author james
%%% @copyright (C) 2017, <DMZK>
%%% @version 17.3.22
%%% @doc
%%% 根据经纬度计算日出、日落等相关时间
%%% @end
%%% Created : 22. 三月 2017 15:48
%%%-------------------------------------------------------------------
-module(lat_lng_time).
-author("james").

%% API
-export([sun_rise_set/4,
  sun_rise_set/3]).


%%====================================================================
%% @doc
%% 参考：https://gist.github.com/junlincao/4badb4e828bdd7e0d112
%% 根据经纬度及日期计算当地的日出日落时间
%% 由于按照算法算出的时区与实际各国划分的时区并不一致，因此不能简单的按经度计算
%% @param
%% Date : {2017,3,13} 要计算的日期
%% Lat　: -90 ~ 90 维度
%% Lng : -180 ~ 180 经度
%% Zone : -12 ~ 14 时区
%% Time : {{6,15},{19,15}}  日出日落时分
%% @end
%%====================================================================
-spec(sun_rise_set(Date :: {integer(), integer(), integer()},
    Lat :: float(), Lng :: float(), Zone :: integer()) ->
  Time :: {{integer(), integer()}, {integer(), integer()}}).
sun_rise_set(Date, Lat, Lng, Zone) ->
  {rise_set_time(Date, 180.0, 0.0, Lat, Lng, Zone, rise), rise_set_time(Date, 180.0, 0.0, Lat, Lng, Zone, set)}.

%%====================================================================
%% @doc
%% 根据经纬度及日期计算日出日落时间
%% 返回UTC标准时间，各时区的时间再进行自行换算
%% @param
%% Date : {2017,3,13} 要计算的日期
%% Lat　: -90 ~ 90 维度
%% Lng : -180 ~ 180 经度
%% Time : {{6,15},{19,15}}  日出日落时分
%% @end
%%====================================================================
-spec(sun_rise_set(Date :: {integer(), integer(), integer()},
    Lat :: float(), Lng :: float()) ->
  Time :: {{integer(), integer()}, {integer(), integer()}}).
sun_rise_set(Date, Lat, Lng) ->
  {rise_set_time(Date, 180.0, 0.0, Lat, Lng, 0, rise), rise_set_time(Date, 180.0, 0.0, Lat, Lng, 0, set)}.


%%====================================================================
%% @private
%% @doc
%% 弧度转角度
%% @end
%%====================================================================
-spec(rad_to_deg(X :: float()) ->
  Degrees :: float()).
rad_to_deg(X) ->
  X * 180 / math:pi().

%%====================================================================
%% @private
%% @doc
%% 角度转弧度
%% @end
%%====================================================================
-spec(deg_to_rad(X :: float()) ->
  Radians :: float()).
deg_to_rad(X) ->
  X * math:pi() / 180.

%%====================================================================
%% @private
%% @doc
%% 根据经纬度、当前日期及时区，计算日出日落时间
%% @end
%%====================================================================
-spec(rise_set_time(
    Date :: {integer(), integer(), integer()},
    UtStart :: float(), UtEnd :: float(), Lat :: float(), Lng :: float(),
    Zone :: integer(), RiseSet :: rise|set) ->
  Time :: {integer(), integer()}).
rise_set_time(_Date, UtStart, UtEnd, _Lat, _Lng, Zone, _RiseSet) when abs(UtEnd - UtStart) < 0.01 ->
  %% 需考虑负数情况
  H = trunc(UtEnd / 15 + Zone) rem 24,
  M = trunc(60 * (UtEnd / 15 + Zone - trunc(UtEnd / 15 + Zone))),
  {(if H >= 0 -> H; true -> 24 + H end),
    (if M >= 0 -> M; true -> 60 + M end)};
rise_set_time(Date, _UtStart, UtEnd, Lat, Lng, Zone, RiseSet) ->
  rise_set_time(Date, UtEnd, Lat, Lng, Zone, RiseSet).
rise_set_time(Date, UtStart, Lat, Lng, Zone, RiseSet) ->
  %% 计算2000-1-1到今天的天数
  {Day, _} = calendar:time_difference({{2000, 1, 1}, {0, 0, 0}}, {Date, {0, 0, 0}}),
  H = math:sin(deg_to_rad(-0.833)),
  T = (Day + UtStart / 360.0) / 36525, %% 世纪数
  L = 280.460 + 36000.777 * T, %% 太阳平均黄径
  G = deg_to_rad(357.528 + 35999.050 * T), %% 太阳平近点角
  Lamda = deg_to_rad(L + 1.915 * math:sin(G) + 0.020 * math:sin(2 * G)), %% 太阳黄道经度
  Epc = deg_to_rad(23.4393 - 0.0130 * T), %% 地球倾角
  Sigam = math:asin(math:sin(Epc) * math:sin(Lamda)), %% 太阳的偏差

%% 格林威治时间太阳时间角
  Gha = UtStart - 180 - 1.915 * math:sin(G) - 0.020 * math:sin(2 * G) +
    2.466 * math:sin(2 * Lamda) - 0.053 * math:sin(4 * Lamda),
%% 修正值e
  E = rad_to_deg(math:acos((H - math:tan(deg_to_rad(Lat)) * math:tan(Sigam)))),
  UtEnd = UtStart - Gha - Lng - (case RiseSet of rise -> E; set -> -E end),
  rise_set_time(Date, UtStart, UtEnd, Lat, Lng, Zone, RiseSet).
