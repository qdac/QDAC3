// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "hostmain.h"
#include "qplugins_loader_lib.hpp"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;

// ---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner) : TForm(Owner)
{
	String APath = ExtractFilePath(Application->ExeName);
	// 缓存下PluginsManager，减少重复调用，毕竟是个函数:)
	_di_IQPluginsManager AMgr = PluginsManager();
	AMgr->Loaders->Add(ToInterface<IQLoader>(new TQBPLLoader(APath, L".bpl")));
	AMgr->Loaders->Add(ToInterface<IQLoader>(new TQDLLLoader(APath, L".dll")));
	AMgr->Start();
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
	wchar_t *DockRoot = L"/Services/Docks/Forms";
	_di_IQServices AServices;
	if (HasInterface(PluginsManager()->ByPath(DockRoot), AServices))
	{
		_di_IQFormService AFormService;
		for (int i = 0; i < AServices->Count; i++)
		{
			if (HasInterface(AServices->Items[i], AFormService))
			{
				if (!AFormService->IsMultiInstance())
				{
					TTabSheet *APage = new TTabSheet(PageControl1);
					APage->PageControl = PageControl1;
					APage->Caption =ToInterface<IQService>(AFormService)->Name;
					APage->Tag = (IntPtr)AFormService.operator->();
					AFormService->DockTo((UIntPtr)APage->Handle,
						APage->ClientRect);
					TQFormEvents AEvents =
					{NULL};
					AEvents.OnFree = DoDockChildFree;
					AFormService->HookEvents(AEvents);
				}
			}
		}
	}
}

// ---------------------------------------------------------------------------
void __fastcall TForm1::DoDockChildFree(_di_IQFormService AService)
{
	IQFormService *AFormService = AService;
	for (int i = 0; i < PageControl1->PageCount; i++)
	{
		if (((void *)PageControl1->Pages[i]->Tag) == AFormService)
		{
			FreeObject(PageControl1->Pages[i]);
			break;
		}
	}
}

void __fastcall TForm1::FormDestroy(TObject * Sender)
{
	for (int i = 0; i < PageControl1->PageCount; i++)
	{
		((IQFormService*)PageControl1->Pages[i]->Tag)->UnhookEvents();
	}
}
// ---------------------------------------------------------------------------
