{ ======================================================
  Registry Editor Implementation By: BitmasterXor

  A Delphi application that provides a GUI interface
  for viewing and editing the Windows Registry.

  Features include:
  - Tree view of registry hierarchy
  - List view of registry values
  - Support for different value types
  - Basic CRUD operations
======================================================== }

unit Unit1;

interface

{ ========== Uses Clause ==========}
uses
  Winapi.Windows,shellapi, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Registry, System.ImageList, Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ImgList,clipbrd,AclApi, AccCtrl;

{ ========== Type Declaration ========== }
type
  { Main Form Class Definition }
  TForm1 = class(TForm)
    { GUI Components }
    MainMenu1: TMainMenu;
    F1: TMenuItem;      // Tools menu
    H1: TMenuItem;      // Help menu
    Edit1: TEdit;       // Path display
    Panel1: TPanel; //splitter for simulating the drag of the reg keys and listview items...
    E2: TMenuItem;
    I1: TMenuItem;
    E3: TMenuItem;      // Exit menu item
    A1: TMenuItem;   // Values display
    ImageList1: TImageList; // Icons
    PopupMenu1: TPopupMenu; // Context menu
    M1: TMenuItem;     // Modify value
    M2: TMenuItem;     // Modify binary
    N1: TMenuItem;     // Separator
    D1: TMenuItem;     // Delete value
    R1: TMenuItem;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    TreeView1: TTreeView;
    ListView1: TListView;
    PopupMenu2: TPopupMenu;
    N2: TMenuItem;
    K1: TMenuItem;
    N3: TMenuItem;
    S1: TMenuItem;
    B1: TMenuItem;
    Q1: TMenuItem;
    Q2: TMenuItem;
    M3: TMenuItem;
    E1: TMenuItem;
    PopupMenu3: TPopupMenu;
    E4: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    D2: TMenuItem;
    R2: TMenuItem;
    N6: TMenuItem;
    E5: TMenuItem;
    N7: TMenuItem;
    C1: TMenuItem;
    K2: TMenuItem;
    N8: TMenuItem;
    S2: TMenuItem;
    B2: TMenuItem;
    D3: TMenuItem;
    Q3: TMenuItem;
    M4: TMenuItem;
    E6: TMenuItem;     // Rename value

    { Event Handlers }
    procedure E3Click(Sender: TObject);              // Exit handler
    procedure FormCreate(Sender: TObject);           // Form initialization
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);                 // Tree node expansion
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);  // Node selection
    procedure M1Click(Sender: TObject);             // Modify value
    procedure M2Click(Sender: TObject);             // Modify binary
    procedure D1Click(Sender: TObject);             // Delete value
    procedure R1Click(Sender: TObject);
    procedure A1Click(Sender: TObject);
    procedure I1Click(Sender: TObject);
    procedure E2Click(Sender: TObject);
    procedure TreeView1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListView1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure K1Click(Sender: TObject);
    procedure S1Click(Sender: TObject);
    procedure B1Click(Sender: TObject);
    procedure Q1Click(Sender: TObject);
    procedure Q2Click(Sender: TObject);
    procedure M3Click(Sender: TObject);
    procedure E1Click(Sender: TObject);
    procedure TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure E4Click(Sender: TObject);
    procedure K2Click(Sender: TObject);
    procedure D2Click(Sender: TObject);
    procedure R2Click(Sender: TObject);
    procedure E5Click(Sender: TObject);
    procedure P1Click(Sender: TObject);
    procedure C1Click(Sender: TObject);
    procedure S2Click(Sender: TObject);
    procedure B2Click(Sender: TObject);
    procedure D3Click(Sender: TObject);
    procedure Q3Click(Sender: TObject);
    procedure M4Click(Sender: TObject);
    procedure E6Click(Sender: TObject);             // Rename value

  private
    Registry: TRegistry;  // Registry access object

    { Private Helper Methods }
    procedure InitializeRegistryTree;    // Initialize root registry keys
    procedure LoadRegistryKeys(const RootKey: HKEY; const Path: string;
      Node: TTreeNode);                 // Load subkeys
    procedure DisplayValues(const KeyPath: string);  // Display key values
    function GetNodePath(Node: TTreeNode): string;  // Get full registry path
    function GetRegDataTypeName(DataType: TRegDataType): string;  // Get type name
    function GetRegDataValue(Registry: TRegistry; const ValueName: string;
      DataType: TRegDataType): string;  // Get formatted value
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses Unit2;

{ ========== Form Initialization ========== }
procedure TForm1.FormCreate(Sender: TObject);
begin
  { Configure TreeView settings }
  TreeView1.Indent := 19;
  TreeView1.ShowRoot := True;




  { Create registry object and initialize tree }
  Registry := TRegistry.Create;
  InitializeRegistryTree;
end;

{ ========== Registry Tree Management ========== }

{ Initialize the root registry tree structure }
{ Import registry file }
{ Import registry file }
procedure TForm1.I1Click(Sender: TObject);
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
begin
  { Configure and show the Open Dialog }
  OpenDialog1.Title := 'Import Registry File';
  OpenDialog1.Filter := 'Registry Files (*.reg)|*.reg|All Files (*.*)|*.*';
  OpenDialog1.DefaultExt := 'reg';

  if OpenDialog1.Execute then
  begin
    { Create the command line }
    CmdLine := Format('cmd.exe /c reg import "%s"', [OpenDialog1.FileName]);

    { Initialize startup info }
    FillChar(StartInfo, SizeOf(TStartupInfo), 0);
    StartInfo.cb := SizeOf(TStartupInfo);
    StartInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartInfo.wShowWindow := SW_HIDE;

    { Execute the command }
    if CreateProcess(nil, PChar(CmdLine), nil, nil, False,
                    CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS,
                    nil, nil, StartInfo, ProcInfo) then
    begin
      { Wait for the process to complete }
      WaitForSingleObject(ProcInfo.hProcess, INFINITE);

      { Get exit code }
      var ExitCode: DWORD;
      GetExitCodeProcess(ProcInfo.hProcess, ExitCode);

      { Clean up }
      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread);

      { Check result }
      if ExitCode = 0 then
      begin
        ShowMessage('Registry file imported successfully');
        InitializeRegistryTree;  // Refresh the tree view
      end
      else
        ShowMessage('Error importing registry file. Exit code: ' + IntToStr(ExitCode));
    end
    else
      ShowMessage('Error executing import command');
  end;
end;

procedure TForm1.InitializeRegistryTree;
var
  ComputerNode, RootNode: TTreeNode;
begin
  TreeView1.Items.Clear;  //ensure the treeview is cleared...

  { Add Computer root node }
  ComputerNode := TreeView1.Items.AddChild(nil, 'Computer');
  ComputerNode.ImageIndex := 0;      // Computer icon
  ComputerNode.SelectedIndex := 0;

  { Add HKEY_CLASSES_ROOT }
  RootNode := TreeView1.Items.AddChild(ComputerNode, 'HKEY_CLASSES_ROOT');
  RootNode.Data := Pointer(HKEY_CLASSES_ROOT);
  RootNode.HasChildren := True;
  RootNode.ImageIndex := 1;          // Folder icon
  RootNode.SelectedIndex := 1;

  { Add HKEY_CURRENT_USER }
  RootNode := TreeView1.Items.AddChild(ComputerNode, 'HKEY_CURRENT_USER');
  RootNode.Data := Pointer(HKEY_CURRENT_USER);
  RootNode.HasChildren := True;
  RootNode.ImageIndex := 1;
  RootNode.SelectedIndex := 1;

  { Add HKEY_LOCAL_MACHINE }
  RootNode := TreeView1.Items.AddChild(ComputerNode, 'HKEY_LOCAL_MACHINE');
  RootNode.Data := Pointer(HKEY_LOCAL_MACHINE);
  RootNode.HasChildren := True;
  RootNode.ImageIndex := 1;
  RootNode.SelectedIndex := 1;

  { Add HKEY_USERS }
  RootNode := TreeView1.Items.AddChild(ComputerNode, 'HKEY_USERS');
  RootNode.Data := Pointer(HKEY_USERS);
  RootNode.HasChildren := True;
  RootNode.ImageIndex := 1;
  RootNode.SelectedIndex := 1;

  { Add HKEY_CURRENT_CONFIG }
  RootNode := TreeView1.Items.AddChild(ComputerNode, 'HKEY_CURRENT_CONFIG');
  RootNode.Data := Pointer(HKEY_CURRENT_CONFIG);
  RootNode.HasChildren := True;
  RootNode.ImageIndex := 1;
  RootNode.SelectedIndex := 1;

  { Expand computer node }
  ComputerNode.Expand(False);
end;

{ Add New Key handler }
procedure TForm1.K1Click(Sender: TObject);
var
  NewName: string;
  Reg: TRegistry;
begin
  NewName := InputBox('New Key', 'Enter key name:', '');
  if NewName <> '' then
  begin
    Reg := TRegistry.Create(KEY_WRITE);
    try
      Reg.RootKey := HKEY(TreeView1.Selected.Data);
      if Reg.OpenKey(GetNodePath(TreeView1.Selected) + '\' + NewName, True) then
      begin
        Reg.CloseKey;
        LoadRegistryKeys(HKEY(TreeView1.Selected.Data), GetNodePath(TreeView1.Selected), TreeView1.Selected);
        TreeView1.Selected.Expand(False);
      end;
    finally
      Reg.Free;
    end;
  end;
end;

// Create new registry key
procedure TForm1.K2Click(Sender: TObject);
var
  NewName: string;
begin
  NewName := InputBox('New Key', 'Enter key name:', '');
  if NewName <> '' then
  begin
    Registry.RootKey := HKEY(TreeView1.Selected.Data);
    if Registry.OpenKey(GetNodePath(TreeView1.Selected) + '\' + NewName, True) then
    begin
      Registry.CloseKey;
      LoadRegistryKeys(HKEY(TreeView1.Selected.Data),
        GetNodePath(TreeView1.Selected), TreeView1.Selected);
      TreeView1.Selected.Expand(False);
    end;
  end;
end;

{ Get the full registry path for a node }
function TForm1.GetNodePath(Node: TTreeNode): string;
begin
  Result := '';
  while (Node <> nil) and (Node.Parent <> nil) and (Node.Parent.Parent <> nil) do
  begin
    if Result = '' then
      Result := Node.Text
    else
      Result := Node.Text + '\' + Result;
    Node := Node.Parent;
  end;
end;

{ ========== TreeView Event Handlers ========== }

{ Handle node expansion }
procedure TForm1.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  { Allow root node expansion }
  if (Node.Level = 0) then
  begin
    AllowExpansion := True;
    Exit;
  end;

  { Load children if not already loaded }
  if (Node.Count = 0) and (Node.Data <> nil) then
  begin
    LoadRegistryKeys(HKEY(Node.Data), GetNodePath(Node), Node);
  end;

  AllowExpansion := True;
end;

procedure TForm1.TreeView1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin

end;

{ Load registry subkeys for a node }
procedure TForm1.ListView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
  begin
  //switching between the different possible popup menus so the user can select to modify stuff or create entirley new stuff...
  if self.ListView1.selected = nil then
  begin
  self.ListView1.PopupMenu:=self.PopupMenu2;
  end
  else
  begin
  self.ListView1.PopupMenu:=self.PopupMenu1;
  end;

end;

procedure TForm1.LoadRegistryKeys(const RootKey: HKEY; const Path: string; Node: TTreeNode);
var
  Reg: TRegistry;
  KeyNames: TStringList;
  i: Integer;
  NewNode: TTreeNode;
begin
  Reg := TRegistry.Create(KEY_READ);
  KeyNames := TStringList.Create;
  try
    Reg.RootKey := RootKey;
    if Reg.OpenKeyReadOnly(Path) then
    begin
      try
        { Get and sort subkey names }
        Reg.GetKeyNames(KeyNames);
        KeyNames.Sort;

        { Add each subkey to the tree }
        for i := 0 to KeyNames.Count - 1 do
        begin
          NewNode := TreeView1.Items.AddChild(Node, KeyNames[i]);
          NewNode.Data := Node.Data;  // Inherit parent's root key
          NewNode.ImageIndex := 1;    // Folder icon
          NewNode.SelectedIndex := 1;

          { Check for subkeys }
          Reg.OpenKeyReadOnly(Path + '\' + KeyNames[i]);
          NewNode.HasChildren := Reg.HasSubKeys;
          Reg.CloseKey;
        end;
      except
        // Handle access denied silently
      end;
      Reg.CloseKey;
    end;
  finally
    KeyNames.Free;
    Reg.Free;
  end;
end;

{ ========== Value Modification Handlers ========== }

{ Modify string value }
procedure TForm1.M1Click(Sender: TObject);
var
  SelectedItem: TListItem;
  Reg: TRegistry;
  NewValue: string;
begin
  if ListView1.Selected = nil then Exit;
  SelectedItem := ListView1.Selected;

  { Validation }
  if SelectedItem = nil then
  begin
    ShowMessage('Please select a value to modify');
    Exit;
  end;

  { Get new value }
  NewValue := InputBox('Modify Value',
    Format('Enter new value for "%s":', [SelectedItem.Caption]),
    SelectedItem.SubItems[1]);

  { Update if changed }
  if NewValue <> SelectedItem.SubItems[1] then
  begin
    Reg := TRegistry.Create(KEY_WRITE);
    try
      Reg.RootKey := HKEY(TreeView1.Selected.Data);
      if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
      begin
        try
          { Write value }
          if SelectedItem.Caption = '(Default)' then
            Reg.WriteString('', NewValue)
          else
            Reg.WriteString(SelectedItem.Caption, NewValue);

          { Refresh display }
          DisplayValues(GetNodePath(TreeView1.Selected));
        except
          ShowMessage('Error writing to registry');
        end;
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

{ Modify binary value }
procedure TForm1.M2Click(Sender: TObject);
var
  SelectedItem: TListItem;
  Reg: TRegistry;
  NewValue: string;
  ByteArray: array of Byte;
  i, Value: Integer;
  Str: TStringList;
begin
  if ListView1.Selected = nil then Exit;
  SelectedItem := ListView1.Selected;

  { Validation }
  if SelectedItem = nil then
  begin
    ShowMessage('Please select a binary value to modify');
    Exit;
  end;

  { Get new hex values }
  NewValue := InputBox('Modify Binary Value',
    Format('Enter new binary data for "%s" (space-separated hex values):',
    [SelectedItem.Caption]), SelectedItem.SubItems[1]);

  if NewValue <> SelectedItem.SubItems[1] then
  begin
    Str := TStringList.Create;
    try
      { Parse hex values }
      Str.Delimiter := ' ';
      Str.DelimitedText := NewValue;
      SetLength(ByteArray, Str.Count);

      { Convert hex strings to bytes }
      for i := 0 to Str.Count - 1 do
      begin
        try
          Value := StrToInt('$' + Str[i]);
          if (Value >= 0) and (Value <= 255) then
            ByteArray[i] := Value
          else
            raise Exception.Create('Invalid byte value');
        except
          ShowMessage('Invalid hex value: ' + Str[i]);
          Exit;
        end;
      end;

      { Write to registry }
      Reg := TRegistry.Create(KEY_WRITE);
      try
        Reg.RootKey := HKEY(TreeView1.Selected.Data);
        if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
        begin
          try
            Reg.WriteBinaryData(SelectedItem.Caption, ByteArray[0], Length(ByteArray));
            DisplayValues(GetNodePath(TreeView1.Selected));
          except
            ShowMessage('Error writing to registry');
          end;
          Reg.CloseKey;
        end;
      finally
        Reg.Free;
      end;
    finally
      Str.Free;
    end;
  end;
end;

{ Add Multi-String Value handler }
procedure TForm1.M3Click(Sender: TObject);
var
  NewName: string;
  Strings: TStringList;
  Reg: TRegistry;
begin
  NewName := InputBox('New Multi-String Value', 'Enter value name:', '');
  if NewName <> '' then
  begin
    Strings := TStringList.Create;
    try
      repeat
        var NewString := InputBox('New Multi-String Value',
          'Enter string value (leave empty to finish):', '');
        if NewString = '' then
          Break;
        Strings.Add(NewString);
      until False;

      if Strings.Count > 0 then
      begin
        Reg := TRegistry.Create(KEY_WRITE);
        try
          Reg.RootKey := HKEY(TreeView1.Selected.Data);
          if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
          begin
            Reg.WriteMultiString(NewName, Strings.ToStringArray);
            DisplayValues(GetNodePath(TreeView1.Selected));
          end;
        finally
          Reg.Free;
        end;
      end;
    finally
      Strings.Free;
    end;
  end;
end;

// Add new multi-string value
procedure TForm1.M4Click(Sender: TObject);
begin
  M3Click(Sender); // Reuse existing multi-string creation
end;


// Show permissions dialog
procedure TForm1.P1Click(Sender: TObject);
begin

end;

{ Add DWORD Value handler }
procedure TForm1.Q1Click(Sender: TObject);
var
  NewName: string;
  NewValue: string;
  IntValue: Integer;
  Reg: TRegistry;
begin
  NewName := InputBox('New DWORD Value', 'Enter value name:', '');
  if NewName <> '' then
  begin
    NewValue := InputBox('New DWORD Value', 'Enter value (decimal or hex with 0x prefix):', '0');
    try
      if Copy(NewValue, 1, 2) = '0x' then
        IntValue := StrToInt('$' + Copy(NewValue, 3, Length(NewValue)))
      else
        IntValue := StrToInt(NewValue);

      Reg := TRegistry.Create(KEY_WRITE);
      try
        Reg.RootKey := HKEY(TreeView1.Selected.Data);
        if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
        begin
          Reg.WriteInteger(NewName, IntValue);
          DisplayValues(GetNodePath(TreeView1.Selected));
        end;
      finally
        Reg.Free;
      end;
    except
      ShowMessage('Invalid number format');
    end;
  end;
end;

{ Add QWORD Value handler }
procedure TForm1.Q2Click(Sender: TObject);
var
  NewName: string;
  NewValue: string;
  IntValue: Int64;
  Reg: TRegistry;
begin
  NewName := InputBox('New QWORD Value', 'Enter value name:', '');
  if NewName <> '' then
  begin
    NewValue := InputBox('New QWORD Value', 'Enter value (decimal or hex with 0x prefix):', '0');
    try
      if Copy(NewValue, 1, 2) = '0x' then
        IntValue := StrToInt64('$' + Copy(NewValue, 3, Length(NewValue)))
      else
        IntValue := StrToInt64(NewValue);

      Reg := TRegistry.Create(KEY_WRITE);
      try
        Reg.RootKey := HKEY(TreeView1.Selected.Data);
        if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
        begin
          Reg.WriteInt64(NewName, IntValue);
          DisplayValues(GetNodePath(TreeView1.Selected));
        end;
      finally
        Reg.Free;
      end;
    except
      ShowMessage('Invalid number format');
    end;
  end;
end;

// Add new QWORD value
procedure TForm1.Q3Click(Sender: TObject);
begin
  Q2Click(Sender); // Reuse existing QWORD creation
end;

{ ========== Value Management ========== }

{ Rename value }
procedure TForm1.R1Click(Sender: TObject);
var
  SelectedItem: TListItem;
  Reg: TRegistry;
  NewName: string;
begin
  if ListView1.Selected = nil then Exit;
  SelectedItem := ListView1.Selected;

  { Validation }
  if SelectedItem = nil then
  begin
    ShowMessage('Please select a value to rename');
    Exit;
  end;

  if SelectedItem.Caption = '(Default)' then
  begin
    ShowMessage('Cannot rename the default value');
    Exit;
  end;

  { Get new name }
  NewName := InputBox('Rename',
    Format('Enter new name for "%s":', [SelectedItem.Caption]),
    SelectedItem.Caption);

  { Process rename }
  if (NewName <> '') and (NewName <> SelectedItem.Caption) then
  begin
    Reg := TRegistry.Create(KEY_ALL_ACCESS);
    try
      Reg.RootKey := HKEY(TreeView1.Selected.Data);
      if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
      begin
        try
          { Copy value to new name }
          { Copy value with correct type }
          case Reg.GetDataType(SelectedItem.Caption) of
            rdString, rdExpandString:
              Reg.WriteString(NewName, Reg.ReadString(SelectedItem.Caption));
            rdInteger:
              Reg.WriteInteger(NewName, Reg.ReadInteger(SelectedItem.Caption));
            rdBinary:
              begin
                var Size := Reg.GetDataSize(SelectedItem.Caption);
                var Buffer: array of Byte;
                SetLength(Buffer, Size);
                Reg.ReadBinaryData(SelectedItem.Caption, Buffer[0], Size);
                Reg.WriteBinaryData(NewName, Buffer[0], Size);
              end;
          end;

          { Delete old value and refresh }
          Reg.DeleteValue(SelectedItem.Caption);
          DisplayValues(GetNodePath(TreeView1.Selected));
        except
          ShowMessage('Error renaming registry value');
        end;
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure TForm1.R2Click(Sender: TObject);
var
  OldName, NewName: string;
  Success: Boolean;
begin
  if TreeView1.Selected = nil then Exit;

  OldName := GetNodePath(TreeView1.Selected);
  NewName := InputBox('Rename Key', 'Enter new name:', TreeView1.Selected.Text);

  if (NewName <> '') and (NewName <> TreeView1.Selected.Text) then
  begin
    Success := False;
    try
      Registry.RootKey := HKEY(TreeView1.Selected.Data);
      Registry.MoveKey(ExtractFilePath(OldName) + TreeView1.Selected.Text,
                      ExtractFilePath(OldName) + NewName, True);
      Success := True;
    except
      Success := False;
    end;

    if Success then
    begin
      TreeView1.Selected.Text := NewName;
      DisplayValues(GetNodePath(TreeView1.Selected));
    end
    else
      ShowMessage('Failed to rename registry key');
  end;
end;

{ Add String Value handler }
procedure TForm1.S1Click(Sender: TObject);
var
  NewName, NewValue: string;
  Reg: TRegistry;
begin
  NewName := InputBox('New String Value', 'Enter value name:', '');
  if NewName <> '' then
  begin
    NewValue := InputBox('New String Value', 'Enter string value:', '');
    Reg := TRegistry.Create(KEY_WRITE);
    try
      Reg.RootKey := HKEY(TreeView1.Selected.Data);
      if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
      begin
        Reg.WriteString(NewName, NewValue);
        DisplayValues(GetNodePath(TreeView1.Selected));
      end;
    finally
      Reg.Free;
    end;
  end;
end;

// Add new string value
procedure TForm1.S2Click(Sender: TObject);
begin
  S1Click(Sender); // Reuse existing string value creation
end;

{ Handle tree view selection change }
procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if Node <> nil then
  begin
    if Node.Level >= 1 then
    begin
      { Display values and update path }
      DisplayValues(GetNodePath(Node));
      Edit1.Text := Node.Text;
      if Node.Level > 1 then
        Edit1.Text := GetNodePath(Node);
    end
    else
    begin
      { Clear display for root node }
      ListView1.Items.Clear;
      Edit1.Text := 'Computer';
    end;
  end;
end;

procedure TForm1.TreeView1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  //check if the user has even selected anything at all....  If COMPUTER is selected DO NOTHING!!!!!
  //ONLY SHOW THE MENU IF HKEY is truly selected!!!!
  if (TreeView1.Selected = nil) or (TreeView1.Selected.Level = 0) then
  begin
    TreeView1.PopupMenu := nil;  // No menu if nothing selected or if Computer node
  end
  else
  begin
    TreeView1.PopupMenu := PopupMenu3;  // Show PopupMenu3 only if HKEY node is selected
  end;
end;

procedure TForm1.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin

end;

{ Delete value }
procedure TForm1.A1Click(Sender: TObject);
begin
form2.showmodal;
end;

{ Add Binary Value handler }
procedure TForm1.B1Click(Sender: TObject);
var
  NewName, HexString: string;
  Reg: TRegistry;
  ByteArray: array of Byte;
  Str: TStringList;
  i, Value: Integer;
begin
  NewName := InputBox('New Binary Value', 'Enter value name:', '');
  if NewName <> '' then
  begin
    HexString := InputBox('New Binary Value', 'Enter binary data (space-separated hex values):', '00');
    Str := TStringList.Create;
    try
      Str.Delimiter := ' ';
      Str.DelimitedText := HexString;
      SetLength(ByteArray, Str.Count);

      for i := 0 to Str.Count - 1 do
      begin
        try
          Value := StrToInt('$' + Str[i]);
          if (Value >= 0) and (Value <= 255) then
            ByteArray[i] := Value
          else raise Exception.Create('Invalid byte value');
        except
          ShowMessage('Invalid hex value: ' + Str[i]);
          Exit;
        end;
      end;

      Reg := TRegistry.Create(KEY_WRITE);
      try
        Reg.RootKey := HKEY(TreeView1.Selected.Data);
        if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
        begin
          Reg.WriteBinaryData(NewName, ByteArray[0], Length(ByteArray));
          DisplayValues(GetNodePath(TreeView1.Selected));
        end;
      finally
        Reg.Free;
      end;
    finally
      Str.Free;
    end;
  end;
end;

// Add new binary value
procedure TForm1.B2Click(Sender: TObject);
begin
  B1Click(Sender); // Reuse existing binary value creation
end;

// Copy key name to clipboard
procedure TForm1.C1Click(Sender: TObject);
begin
  Clipboard.AsText := GetNodePath(TreeView1.Selected);
end;

procedure TForm1.D1Click(Sender: TObject);
var
  SelectedItem: TListItem;
  Reg: TRegistry;
begin
  if ListView1.Selected = nil then Exit;
  SelectedItem := ListView1.Selected;

  { Validation }
  if SelectedItem = nil then
  begin
    ShowMessage('Please select a value to delete');
    Exit;
  end;

  if SelectedItem.Caption = '(Default)' then
  begin
    ShowMessage('Cannot delete the default value');
    Exit;
  end;

  { Confirm deletion }
  if MessageDlg(Format('Are you sure you want to delete "%s"?',
     [SelectedItem.Caption]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Reg := TRegistry.Create(KEY_ALL_ACCESS);
    try
      Reg.RootKey := HKEY(TreeView1.Selected.Data);
      if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
      begin
        try
          { Delete and refresh }
          Reg.DeleteValue(SelectedItem.Caption);
          DisplayValues(GetNodePath(TreeView1.Selected));
        except
          ShowMessage('Error deleting registry value');
        end;
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

// Delete selected registry key
procedure TForm1.D2Click(Sender: TObject);
var
  KeyPath: string;
begin
  if MessageDlg('Delete key ' + GetNodePath(TreeView1.Selected) + ' and all subkeys?',
    mtWarning, [mbYes, mbNo], 0) = mrYes then
  begin
    KeyPath := GetNodePath(TreeView1.Selected);
    Registry.RootKey := HKEY(TreeView1.Selected.Data);
    if Registry.DeleteKey(KeyPath) then
    begin
      TreeView1.Selected.Delete;
      ListView1.Items.Clear;
    end;
  end;
end;

// Add new DWORD value
procedure TForm1.D3Click(Sender: TObject);
begin
  Q1Click(Sender); // Reuse existing DWORD creation
end;

{ ========== Value Display and Formatting ========== }

{ Display registry values in ListView }
procedure TForm1.DisplayValues(const KeyPath: string);
var
  Reg: TRegistry;
  ValueNames: TStringList;
  i: Integer;
  ValueType: TRegDataType;
  Item: TListItem;
begin
  ListView1.Items.Clear;
  if TreeView1.Selected = nil then Exit;
  if TreeView1.Selected.Level < 1 then Exit; // Skip Computer node

  Reg := TRegistry.Create(KEY_READ);
  ValueNames := TStringList.Create;
  try
    Reg.RootKey := HKEY(TreeView1.Selected.Data);
    if Reg.OpenKeyReadOnly(KeyPath) then
    begin
      try
        { Add default value first }
        Item := ListView1.Items.Add;
        Item.Caption := '(Default)';
        Item.SubItems.Add('REG_SZ');
        Item.ImageIndex := 3;  // String value icon
        try
          Item.SubItems.Add(Reg.ReadString(''));
        except
          Item.SubItems.Add('(value not set)');
        end;

        { Add other values }
        Reg.GetValueNames(ValueNames);
        ValueNames.Sort;
        for i := 0 to ValueNames.Count - 1 do
        begin
          if ValueNames[i] <> '' then
          begin
            ValueType := Reg.GetDataType(ValueNames[i]);
            Item := ListView1.Items.Add;
            Item.Caption := ValueNames[i];
            Item.SubItems.Add(GetRegDataTypeName(ValueType));
            Item.SubItems.Add(GetRegDataValue(Reg, ValueNames[i], ValueType));

            { Set appropriate icon }
            case ValueType of
              rdString, rdExpandString, rdMultiString:
                Item.ImageIndex := 3;  // String value icon
              rdInteger, rdBinary:
                Item.ImageIndex := 2;  // Binary value icon
              else
                Item.ImageIndex := 2;  // Default to binary icon
            end;
          end;
        end;
      except
        // Handle errors silently
      end;
      Reg.CloseKey;
    end;
  finally
    ValueNames.Free;
    Reg.Free;
  end;
end;

{ Get registry data type name }
function TForm1.GetRegDataTypeName(DataType: TRegDataType): string;
begin
  case DataType of
    rdString, rdExpandString: Result := 'REG_SZ';
    rdInteger: Result := 'REG_DWORD';
    rdBinary: Result := 'REG_BINARY';
    rdMultiString: Result := 'REG_MULTI_SZ';
    else Result := 'Unknown';
  end;
end;

{ Format registry value for display }
function TForm1.GetRegDataValue(Registry: TRegistry; const ValueName: string;
  DataType: TRegDataType): string;
var
  Buffer: array of Byte;
  Size: Integer;
  i: Integer;
begin
  Result := '';
  try
    case DataType of
      rdString, rdExpandString:
        Result := Registry.ReadString(ValueName);
      rdInteger:
        begin
          { Format as hex and decimal }
          Result := Format('0x%.8x', [Registry.ReadInteger(ValueName)]) +
                   Format(' (%d)', [Registry.ReadInteger(ValueName)]);
        end;
      rdBinary:
        begin
          { Format as hex bytes }
          Size := Registry.GetDataSize(ValueName);
          SetLength(Buffer, Size);
          Registry.ReadBinaryData(ValueName, Buffer[0], Size);
          for i := 0 to Min(Size - 1, 16) do
          begin
            Result := Result + IntToHex(Buffer[i], 2) + ' ';
          end;
          if Size > 16 then
            Result := Result + '...';
        end;
      rdMultiString:
        Result := String.Join(' ', Registry.ReadMultiString(ValueName));
    end;
  except
    Result := '(Cannot read value)';
  end;
end;

{ ========== Application Control ========== }

{ Exit application }
{ Export registry key }
{ Export registry key }
{ Add Expandable String Value handler }
procedure TForm1.E1Click(Sender: TObject);
var
  NewName, NewValue: string;
  Reg: TRegistry;
begin
  NewName := InputBox('New Expandable String Value', 'Enter value name:', '');
  if NewName <> '' then
  begin
    NewValue := InputBox('New Expandable String Value',
      'Enter string value (use %VAR% for environment variables):', '');
    Reg := TRegistry.Create(KEY_WRITE);
    try
      Reg.RootKey := HKEY(TreeView1.Selected.Data);
      if Reg.OpenKey(GetNodePath(TreeView1.Selected), False) then
      begin
        Reg.WriteExpandString(NewName, NewValue);
        DisplayValues(GetNodePath(TreeView1.Selected));
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure TForm1.E2Click(Sender: TObject);
var
  StartInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
  FullKeyPath: string;
begin
  { Ensure a key is selected }
  if (TreeView1.Selected = nil) or (TreeView1.Selected.Level < 1) then
  begin
    ShowMessage('Please select a registry key to export.');
    Exit;
  end;

  { Build the full registry key path }
  if TreeView1.Selected.Level = 1 then
    FullKeyPath := TreeView1.Selected.Text
  else
    FullKeyPath := TreeView1.Selected.Parent.Text + '\' + GetNodePath(TreeView1.Selected);

  { Configure and show the Save Dialog }
  SaveDialog1.Title := 'Export Registry Key';
  SaveDialog1.Filter := 'Registry Files (*.reg)|*.reg|All Files (*.*)|*.*';
  SaveDialog1.DefaultExt := 'reg';
  SaveDialog1.FileName := StringReplace(GetNodePath(TreeView1.Selected), '\', '_', [rfReplaceAll]) + '.reg';

  if SaveDialog1.Execute then
  begin
    { Create the command line }
    CmdLine := Format('cmd.exe /c reg export "%s" "%s" /y',
                     [FullKeyPath, SaveDialog1.FileName]);

    { Initialize startup info }
    FillChar(StartInfo, SizeOf(TStartupInfo), 0);
    StartInfo.cb := SizeOf(TStartupInfo);
    StartInfo.dwFlags := STARTF_USESHOWWINDOW;
    StartInfo.wShowWindow := SW_HIDE;

    { Execute the command }
    if CreateProcess(nil, PChar(CmdLine), nil, nil, False,
                    CREATE_NO_WINDOW or NORMAL_PRIORITY_CLASS,
                    nil, nil, StartInfo, ProcInfo) then
    begin
      { Wait for the process to complete }
      WaitForSingleObject(ProcInfo.hProcess, INFINITE);

      { Get exit code }
      var ExitCode: DWORD;
      GetExitCodeProcess(ProcInfo.hProcess, ExitCode);

      { Clean up }
      CloseHandle(ProcInfo.hProcess);
      CloseHandle(ProcInfo.hThread);

      { Check result }
      if ExitCode = 0 then
        ShowMessage('Registry key exported successfully to ' + SaveDialog1.FileName)
      else
        ShowMessage('Error exporting registry key. Exit code: ' + IntToStr(ExitCode));
    end
    else
      ShowMessage('Error executing export command');
  end;
end;

procedure TForm1.E3Click(Sender: TObject);
begin
  if Registry <> nil then
    Registry.Free;
  halt;
end;

// Expand selected node
procedure TForm1.E4Click(Sender: TObject);
begin
  if TreeView1.Selected <> nil then
    TreeView1.Selected.Expand(False);
end;

// Export selected key
procedure TForm1.E5Click(Sender: TObject);
begin
  E2Click(Sender); // Reuse existing export functionality
end;


// Add new expandable string value
procedure TForm1.E6Click(Sender: TObject);
begin
  E1Click(Sender); // Reuse existing expandable string creation
end;

end.
