//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "shellapi.h"
#include "qworker.hpp"
#include "qjson.hpp"
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include "NxColumnClasses.hpp"
#include "NxColumns.hpp"
#include "NxCustomGrid.hpp"
#include "NxCustomGridControl.hpp"
#include "NxGrid.hpp"
#include "NxScrollControl.hpp"
#include <Vcl.Dialogs.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------
class TForm_Main : public TForm
{
__published:	// IDE-managed Components
	TPanel *P_Work;
	TPageControl *PC;
	TTabSheet *TabReadme;
	TTabSheet *TabProject;
	TMemo *M;
	TPanel *P1;
	TPanel *Panel2;
	TPanel *Panel3;
	TPanel *Panel4;
	TSplitter *Splitter1;
	TPageControl *CompilerPC;
	TPanel *Panel5;
	TNextGrid *NG;
	TNxIncrementColumn *NGNo;
	TNxCheckBoxColumn *NGUse;
	TNxTextColumn *NGFile;
	TOpenDialog *OD;
	TSaveDialog *SD;
	TPanel *Panel6;
	TPanel *Panel7;
	TButton *BtnAdd;
	TButton *BtnDelete;
	TButton *BtnDown;
	TButton *BtnUp;
	TButton *BtnSave;
	TButton *BtnCompiler;
	TPageControl *ProjectSet;
	TTabSheet *TabOutPath;
	TTabSheet *TabProjectSet;
	TLabeledEdit *OutFinalDir;
	TButton *Button1;
	TLabeledEdit *OutIncludeDir;
	TButton *Button2;
	TLabeledEdit *OutLibDir;
	TButton *Button3;
	TLabeledEdit *ExtraAlias;
	TLabeledEdit *ExtraIncludeDir;
	TLabeledEdit *ExtraLibDir;
	TButton *Button4;
	TButton *Button5;
	TLabeledEdit *ExtraSearchDir;
	TButton *Button6;
	TTabSheet *TabLog;
	TPanel *P2;
	TMemo *Log;
	TButton *BtnSaveAs;
	TTabSheet *TabNew;
	TLabeledEdit *CustomRootDir;
	TButton *Button7;
	TButton *AddIDE;
	TLabel *L;
	TPopupMenu *PM;
	TMenuItem *PM_SelectAll;
	TMenuItem *PM_SelectColumn;
	TMenuItem *PM_ClearColumn;
	TMenuItem *PM_ClearAll;
	TMenuItem *PM_DeSelectAll;
	TMenuItem *PM_DeSelectColumn;
	TCheckBox *ShowCmd;
	TButton *BtnSaveLog;
	TButton *BtnClearLog;
	TCheckBox *ShowAllLog;
	TPanel *P3;
	TButton *BtnRegType;
	TLabel *L_Dir;
	TButton *BtnOpen;
	TRadioButton *BuildRelease;
	TRadioButton *BuildDebug;
	TCheckBox *ExtraNameSpaces;
	TCheckBox *DpkAddIDEVersion;
	TMenuItem *PM_CompilerSort;
  TMenuItem *PM_NoCompilerByRed;
	TLabeledEdit *ExtraNameSpaceSearch;
	TCheckBox *ExtraBuildAllUnits;
	TMenuItem *PM_All;
	TMenuItem *PM_Row;
	TMenuItem *PM_Column;
	TMenuItem *PM_SelectRow;
	TMenuItem *PM_ClearRow;
	TMenuItem *PM_DeSelectRow;
	TComboBox *ChangeLangID;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall BtnAddClick(TObject *Sender);
	void __fastcall BtnDeleteClick(TObject *Sender);
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
	void __fastcall Button3Click(TObject *Sender);
	void __fastcall Button5Click(TObject *Sender);
	void __fastcall Button4Click(TObject *Sender);
	void __fastcall Button6Click(TObject *Sender);
	void __fastcall BtnSaveClick(TObject *Sender);
	void __fastcall BtnSaveAsClick(TObject *Sender);
	void __fastcall Button7Click(TObject *Sender);
	void __fastcall AddIDEClick(TObject *Sender);
	void __fastcall NGMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y);
	void __fastcall PM_SelectAllClick(TObject *Sender);
	void __fastcall PM_ClearAllClick(TObject *Sender);
	void __fastcall PM_SelectColumnClick(TObject *Sender);
	void __fastcall PM_ClearColumnClick(TObject *Sender);
	void __fastcall PM_DeSelectAllClick(TObject *Sender);
	void __fastcall PM_DeSelectColumnClick(TObject *Sender);
	void __fastcall PM_SelectRowClick(TObject *Sender);
	void __fastcall PM_ClearRowClick(TObject *Sender);
	void __fastcall PM_DeSelectRowClick(TObject *Sender);
	void __fastcall BtnCompilerClick(TObject *Sender);
	void __fastcall BtnClearLogClick(TObject *Sender);
	void __fastcall BtnSaveLogClick(TObject *Sender);
	void __fastcall BtnDownClick(TObject *Sender);
	void __fastcall BtnUpClick(TObject *Sender);
	void __fastcall BtnRegTypeClick(TObject *Sender);
	void __fastcall BtnOpenClick(TObject *Sender);
	void __fastcall TabLogShow(TObject *Sender);
	void __fastcall TabProjectShow(TObject *Sender);
	void __fastcall TabReadmeShow(TObject *Sender);
	void __fastcall PM_CompilerSortClick(TObject *Sender);
  void __fastcall PM_NoCompilerByRedClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
	void __fastcall ChangeLangIDChange(TObject *Sender);
private:	// User declarations
	UnicodeString CfgName,DotNETPath,ComSpec,CurrentDir;
	TStringList *RegPath;
	TStringList *ExtList;
	TStringList *CompilerList;
	TStringList *FileList;
	TQJson *CustomCfg;
	TQJobGroup *JobGroup;
	UTF8String FullLogTxt;
	bool OnlyFromConfig;
	LANGID SelectLangID;
	void __fastcall GetDotNETPath();
	void __fastcall GetCompilerFromReg();
	void __fastcall AcceptFiles(TMessage& Msg);
	void __fastcall CheckCompiler(TQJson *QJson);
	UnicodeString __fastcall PipeCall(UnicodeString CmdLine,UnicodeString Path="");
	void __fastcall CreateTabSheet(TQJson *QJson);
	void __fastcall GetCompilerInfo(TQJson *QJson);
	int __fastcall GetDelphiIDEVersion(TQJson *QJson);
	int __fastcall AddFile(UnicodeString FileName);
	void __fastcall GetEnvVar(TQJson *QJson);
	void __fastcall SaveProject(UnicodeString FileName);
	void __fastcall LoadProject(UnicodeString FileName);
	void __fastcall GetOnlyFromConfig();
	void __fastcall GetCompilerFromConfig();
	void __fastcall SetColumnByExt(UnicodeString ColumnName,UnicodeString Platform);
	void __fastcall CheckColumnByExt();
	void __fastcall SetCompilerResult(UnicodeString Result);
	void __fastcall ShowCompilerResult(PQJob AJob);
	UnicodeString __fastcall GetIncludePaths(TQJson *QJson);
	UnicodeString __fastcall GetLibPaths(TQJson *QJson,bool IsRelease=true);
	UnicodeString __fastcall GetSearchPaths(TQJson *QJson);
	void __fastcall CompilerJob(PQJob AJob);
	UnicodeString __fastcall FilterResult(UnicodeString Result);
	void __fastcall RegisterFileType(UnicodeString ExtName,UnicodeString AppName);
	bool __fastcall FileTypeIsRegister(UnicodeString ExtName);
	UnicodeString __fastcall ReplPlatformPath(UnicodeString Path,TQJson *QJson);
	void __fastcall CheckResult(UnicodeString Result,UnicodeString FileName,UnicodeString ColumnName);
	void __fastcall ShowResultOnGrid(PQJob AJob);
	void __fastcall CheckDPKSuffix(UnicodeString &FileName,int IDEVersion);
	void __fastcall GetCurrentSort(TQJson *QJson);
	void __fastcall GetNewSort(TQJson *QJson);
	void __fastcall SortToNG(TQJson *QJson);
  void __fastcall DelayRunWithCmdline(PQJob AJob);
  bool __fastcall RunWithCmdline();
  void __fastcall ScanFiles(UnicodeString Source,TStrings *Masks);
  void __fastcall SelectVerAndPlatform(UnicodeString Ver,UnicodeString Platform);
  void __fastcall GenerateHTMLReport(UnicodeString Path);
  void __fastcall DoLanguageChanged(TObject *ASender);
public:		// User declarations
	__fastcall TForm_Main(TComponent* Owner);
  MESSAGE void __fastcall SystemMenuCommand(TWMMenuSelect &Msg);
BEGIN_MESSAGE_MAP
	MESSAGE_HANDLER(WM_DROPFILES,TMessage,AcceptFiles)
  MESSAGE_HANDLER(WM_SYSCOMMAND,TWMMenuSelect,SystemMenuCommand);
END_MESSAGE_MAP(TForm)
};
//---------------------------------------------------------------------------
extern TForm_Main *Form_Main;
//---------------------------------------------------------------------------
UnicodeString __fastcall FolderBrowser(UnicodeString CurrPath);
//---------------------------------------------------------------------------
UnicodeString __fastcall Repl(UnicodeString Str,UnicodeString Old,UnicodeString New);
//---------------------------------------------------------------------------
UnicodeString __fastcall GetQuotePaths(UnicodeString Paths,bool Quote=false);
//---------------------------------------------------------------------------
UnicodeString __fastcall GetCompleteFileName(UnicodeString CurrPath,UnicodeString FileName);
//---------------------------------------------------------------------------
int __fastcall IsFMX(UnicodeString FileName);
//---------------------------------------------------------------------------
int __fastcall IsNoVCL(UnicodeString FileName);
//---------------------------------------------------------------------------
bool __fastcall IsDesignOnly(UnicodeString FileName);
//---------------------------------------------------------------------------
#endif
//---------------------------------------------------------------------------
