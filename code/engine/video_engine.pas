unit video_engine;

interface

implementation

{

  uses
  System.Classes,
  System.SysUtils,
  System.Types,
  FMX.Graphics,
  FMX.Controls,
  libavcodec_avfft,
  libavformat_avio,
  libavutil_frame;

  type
  TFFmpegPlayer = class(TControl)
  private
  FFileName: string;
  FFrame: PAVFrame;
  FFrameSize: Integer;
  FStream: ;
  FCodec: PAVCodec;
  FCodecContext: PAVCodecContext;
  FFormatContext: PAVFormatContext;
  FPacket: TAVPacket;
  FLastFrameTime: Int64;
  FTimer: TTimer;
  FVideoStarted: Boolean;
  FVideoEnded: Boolean;
  FOnEndOfVideo: TNotifyEvent;
  FOnNewFrame: TNotifyEvent;
  procedure SetFileName(const Value: string);
  procedure FreeResources;
  procedure InitializeCodecContext;
  procedure DecodePacket;
  procedure DisplayFrame;
  procedure HandleTimer(Sender: TObject);
  protected
  procedure Paint; override;
  public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure Play;
  procedure Stop;
  property FileName: string read FFileName write SetFileName;
  property OnEndOfVideo: TNotifyEvent read FOnEndOfVideo write FOnEndOfVideo;
  property OnNewFrame: TNotifyEvent read FOnNewFrame write FOnNewFrame;
  end;

  implementation

  constructor TFFmpegPlayer.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FFileName := '';
  FFrame := nil;
  FFrameSize := 0;
  FStream := nil;
  FCodec := nil;
  FCodecContext := nil;
  FFormatContext := nil;
  av_init_packet(@FPacket);
  FLastFrameTime := 0;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 1;
  FTimer.Enabled := False;
  FTimer.OnTimer := HandleTimer;
  FVideoStarted := False;
  FVideoEnded := False;
  end;

  destructor TFFmpegPlayer.Destroy;
  begin
  FreeResources;
  av_packet_unref(@FPacket);
  FTimer.Free;
  inherited Destroy;
  end;

  procedure TFFmpegPlayer.SetFileName(const Value: string);
  begin
  if FFileName <> Value then
  begin
  FFileName := Value;
  if FVideoStarted then
  Stop;
  end;
  end;

  procedure TFFmpegPlayer.FreeResources;
  begin
  if FCodecContext <> nil then
  begin
  avcodec_close(FCodecContext);
  avcodec_free_context(@FCodecContext);
  FCodecContext := nil;
  end;
  if FFormatContext <> nil then
  begin
  avformat_close_input(@FFormatContext);
  FFormatContext := nil;
  end;
  if FFrame <> nil then
  begin
  av_frame_unref(FFrame);
  av_frame_free(@FFrame);
  FFrame := nil;
  FFrameSize := 0;
  end;
  FStream := nil;
  FCodec := nil;
  FLastFrameTime := 0;
  FVideoStarted := False;
  FVideoEnded := False;
  end;

  procedure TFFmpegPlayer.InitializeCodecContext;
  var
  i: Integer;
  begin
  FCodec := nil;
  FCodecContext := nil;
  for i := 0 to FFormatContext^.nb_streams - 1 do
  begin
  if FFormatContext^.streams[i].codecpar.codec_type = AVMEDIA_TYPE_VIDEO then
  begin
  FStream := FFormatContext^.streams[i];
  FCodec := avcodec_find_decoder(FStream.codecpar.codec_id);
  if FCodec <> nil then
  begin
  FCodecContext := avcodec_alloc_context3(FCodec);
  if avcodec_parameters_to_context(FCodecContext, FStream.codecpar) >= 0 then
  begin
  if avcodec_open2(FCodecContext, FCodec, nil) = 0 then
  begin
  FFrame := av_frame_alloc;
  FFrameSize := av_image_get_buffer_size(AV_PIX_FMT_RGB24, FCodecContext^.width,
  FCodecContext^.height, 1);
  av_image_alloc(FFrame^.data, FFrame^.linesize, FCodecContext^.width,
  FCodecContext^.height, AV_PIX_FMT_RGB24, 1);
  FLastFrameTime := 0;
  FTimer.Enabled := True;
  FVideoStarted := True;
  FVideoEnded := False;
  end
  else
  begin
  avcodec_free_context(@FCodecContext);
  FCodecContext := nil;
  end;
  end;
  end;
  Break;
  end;
  end;
  end;

  procedure TFFmpegPlayer.DecodePacket;
  var
  res: Integer;
  begin
  res := avcodec_send_packet(FCodecContext, @FPacket);
  if res = 0 then
  begin
  while res >= 0 do
  begin
  res := avcodec_receive_frame(FCodecContext, FFrame);
  if res = 0 then
  begin
  DisplayFrame;
  av_frame_unref(FFrame);
  end;
  end;
  end;
  end;

  procedure TFFmpegPlayer.DisplayFrame;
  begin
  if Assigned(FOnNewFrame) then
  FOnNewFrame(Self);
  end;

  procedure TFFmpegPlayer.HandleTimer(Sender: TObject);
  var
  pktRead: Boolean;
  pktPts: Int64;
  begin
  if FFormatContext = nil then
  begin
  FFormatContext := avformat_alloc_context;
  avformat_open_input(@FFormatContext, PAnsiChar(AnsiString(FFileName)), nil, nil);
  avformat_find_stream_info(FFormatContext, nil);
  InitializeCodecContext;
  end;

  pktRead := False;
  while not pktRead do
  begin
  if av_read_frame(FFormatContext, @FPacket) >= 0 then
  begin
  if FPacket.stream_index = FStream.index then
  begin
  pktPts := av_rescale_q(FPacket.pts, FStream.time_base, AV_TIME_BASE_Q);
  if (pktPts >= FLastFrameTime) or (FLastFrameTime = 0) then
  begin
  FLastFrameTime := pktPts;
  DecodePacket;
  pktRead := True;
  end;
  end;
  av_packet_unref(@FPacket);
  end
  else
  begin
  if not FVideoEnded then
  begin
  FVideoEnded := True;
  if Assigned(FOnEndOfVideo) then
  FOnEndOfVideo(Self);
  end;
  Break;
  end;
  end;
  end;

  procedure TFFmpegPlayer.Paint;
  begin
  inherited Paint;
  if FFrame <> nil then
  begin
  Canvas.DrawBitmap(TBitmap.Wrap(FCodecContext^.width, FCodecContext^.height, PF_RGB_24BIT,
  FFrame^.data, FFrame^.linesize), LocalRect, 1.0);
  if Assigned(FOnNewFrame) then
  FOnNewFrame(Self);
  end;
  end;

  procedure TFFmpegPlayer.Play;
  begin
  if FFileName = '' then
  raise Exception.Create('File name not set');
  if FVideoStarted then
  Exit;
  try
  FreeResources;
  if avformat_open_input(@FFormatContext, PAnsiChar(AnsiString(FFileName)), nil, nil) < 0 then
  raise Exception.Create('Could not open file');
  if avformat_find_stream_info(FFormatContext, nil) < 0 then
  raise Exception.Create('Could not find stream information');
  av_dump_format(FFormatContext, 0, PAnsiChar(AnsiString(FFileName)), 0);
  FStream := av_find_best_stream(FFormatContext, AVMEDIA_TYPE_VIDEO, -1, -1, @FCodec, 0);
  if FStream = nil then
  raise Exception.Create('Could not find video stream');
  FCodecContext := avcodec_alloc_context3(FCodec);
  avcodec_parameters_to_context(FCodecContext, FStream^.codecpar);
  if avcodec_open2(FCodecContext, FCodec, nil) < 0 then
  raise Exception.Create('Could not open codec');
  FFrame := av_frame_alloc();
  FFrameSize := av_image_get_buffer_size(AV_PIX_FMT_RGB24, FCodecContext^.width,
  FCodecContext^.height, 1);
  av_image_alloc(FFrame^.data, FFrame^.linesize, FCodecContext^.width, FCodecContext^.height,
  AV_PIX_FMT_RGB24, 1);
  FPacket.data := nil;
  FPacket.size := 0;
  FLastFrameTime := 0;
  FTimer.Enabled := True;
  FVideoStarted := True;
  FVideoEnded := False;
  except
  on E: Exception do
  begin
  FreeResources;
  raise;
  end;
  end;
  end;

  procedure TFFmpegPlayer.Stop;
  begin
  if not FVideoStarted then
  Exit;
  FTimer.Enabled := False;
  FreeResources;
  if Assigned(FOnEndOfVideo) then
  FOnEndOfVideo(Self);
  end;

  procedure TFFmpegPlayer.HandleTimer(Sender: TObject);
  var
  currentTime: Int64;
  begin
  DecodePacket;
  if FPacket.data = nil then
  begin
  if not FVideoEnded then
  begin
  FVideoEnded := True;
  Stop;
  end;
  Exit;
  end;
  currentTime := av_rescale_q(FFrame^.pts, FStream^.time_base, AV_TIME_BASE_Q);
  if (FLastFrameTime = 0) or (currentTime - FLastFrameTime >= 0) then
  begin
  DisplayFrame;
  FLastFrameTime := currentTime;
  end;
  av_packet_unref(@FPacket);
  end;

  procedure TFFmpegPlayer.DecodePacket;
  var
  frameFinished: Integer;
  begin
  while (FPacket.size > 0) and (not FVideoEnded) do
  begin
  avcodec_send_packet(FCodecContext, @FPacket);
  frameFinished := 0;

  while (not FVideoEnded) and (frameFinished = 0) do
  begin
  avcodec_receive_frame(FCodecContext, FFrame);
  frameFinished := avcodec_send_packet(FCodecContext, @FPacket);

  if (frameFinished >= 0) then
  begin
  DisplayFrame;
  end
  else if (frameFinished = AVERROR_EOF) then
  begin
  FVideoEnded := True;
  if Assigned(FOnEndOfVideo) then
  FOnEndOfVideo(Self);
  end
  else if (frameFinished = AVERROR(EAGAIN)) then
  begin
  Break;
  end
  else
  begin
  raise Exception.Create('Error while decoding frame');
  end;
  end;

  av_packet_unref(@FPacket);
  end;
  end;
  if frameFinished = 0 then
  begin
  // the decoder has consumed the packet but has not produced a frame yet
  Exit;
  end;

  // decode the frame
  avcodec_receive_frame(FCodecContext, FFrame);
  if FFrame^.format <> AV_PIX_FMT_RGB24 then
  begin
  // convert the frame to RGB24 format
  av_image_alloc(@FFrame^.data, @FFrame^.linesize, FCodecContext^.width, FCodecContext^.height,
  AV_PIX_FMT_RGB24, 1);
  FFrameSize := av_image_get_buffer_size(AV_PIX_FMT_RGB24, FCodecContext^.width,
  FCodecContext^.height, 1);
  SwsContext := sws_getContext(FCodecContext^.width, FCodecContext^.height, FCodecContext^.pix_fmt,
  FCodecContext^.width, FCodecContext^.height, AV_PIX_FMT_RGB24, SWS_BICUBIC, nil, nil, nil);
  sws_scale(SwsContext, FFrame^.data, FFrame^.linesize, 0, FCodecContext^.height, FFrame^.data,
  FFrame^.linesize);
  sws_freeContext(SwsContext);
  end;
  FLastFrameTime := FPacket.pts;
  if Assigned(FOnNewFrame) then
  FOnNewFrame(Self);
  end;

  procedure TFFmpegPlayer.DisplayFrame;
  begin
  Invalidate;
  end;

  procedure TFFmpegPlayer.HandleTimer(Sender: TObject);
  begin
  DecodePacket;
  DisplayFrame;
  end;

  procedure TFFmpegPlayer.Play;
  begin
  if FFileName = '' then
  Exit;

  FreeResources;

  // open the input file and read the format header
  if avformat_open_input(@FFormatContext, PAnsiChar(AnsiString(FFileName)), nil, nil) < 0 then
  Exit;
  if avformat_find_stream_info(FFormatContext, nil) < 0 then
  Exit;

  // find the first video stream and its decoder
  FStream := nil;
  FCodec := nil;
  for var i := 0 to FFormatContext^.nb_streams - 1 do
  begin
  if FFormatContext^.streams[i]^.codecpar^.codec_type = AVMEDIA_TYPE_VIDEO then
  begin
  FStream := FFormatContext^.streams[i];
  FCodec := avcodec_find_decoder(FStream^.codecpar^.codec_id);
  if FCodec = nil then
  Exit;
  end; // allocate and open the codec context
  FCodecContext := avcodec_alloc_context3(FCodec);
  if avcodec_parameters_to_context(FCodecContext, FStream^.codecpar) < 0 then
  Exit;
  if avcodec_open2(FCodecContext, FCodec, nil) < 0 then
  Exit;

  // allocate the frame
  FFrame := av_frame_alloc;
  if FFrame = nil then
  Exit;

  // start the timer
  FTimer.Enabled := True;
  FVideoStarted := True;
  Exit;
  end;
  end;
  end;

  procedure TFFmpegPlayer.Stop;
  begin
  if FVideoStarted then
  begin
  FTimer.Enabled := False;
  FreeResources;
  if Assigned(FOnEndOfVideo) then
  FOnEndOfVideo(Self);
  end;
  end;
}
end.
