# QDAC
QDAC 项目在Github 上的一个备份，详情请见官网：http://www.qdac.cc/ 以及官方SVN：svn://www.qdac.cc/

# QDAC项目简介

QDAC 是由 QDAC 开发组为大家提供的一套 Delphi/C++ Builder 跨平台开源组件库，名称取自英文 Quick Data Access Components 的首字母。它支持 Delphi/C++ Builder 2007 以上的版本的开发环境，目标是为大家提供一套高效稳定的跨平台快速数据访问组件，替代系统实现的低效组件。

既然是快速数据访问组件，QDAC 对速度会比较重视，但出于跨平台的考虑，QDAC 是用纯 Pascal 源码编写，没有使用任何汇编代码，所以性能上，理论上不会比使用纯汇编优化的版本强，但也远远高于一般的实现。同时，QDAC 也很重视稳定性，希望大家多多参与测试，保证在速度优化的同时，稳定性上不会存在问题。

QDAC 项目目前包含以下组件（QDB、QSocket、QScript 目前未完成）：

QWorker
QWorker 是一个基于作业视角的跨平台并行编程框架，提供丰富的功能和接口。进一步了解可以浏览 QWorker 专题 。

QJson
QJson 是一套快速方便，兼容性好的 JSON 格式跨平台管理单元，提供丰富功能的接口支持。与 SuperObject 等方案相比，速度更胜一筹。进一步了解可以浏览 QJson 专题 的内容。

QXML
QXML 是一套快速方便，兼容性好的 XML 格式跨平台管理单元，提供丰富功能的接口支持。与 NativeXML 等方案相比，速度更胜一筹。更详细的信息，可以参考源码中相关注释说明。

QMsgPack
QMsgPack 是一套快速方便，兼容性好的 Message Pack 协议跨平台管理单元，提供丰富功能的接口支持，并完整支持 Message Pack 协议的扩展数据类型。速度依然很快，而且是目前 Delphi 下最完整的 Message Pack 协议实现。进一步了解可以浏览 QMsgPack 专题 的内容。

QLog
QLog 是一套跨平台的异步日志记录单元，支持Linux 标准的 SyslogD 协议。通过采用异步方式，将对程序速度的影响降低到最低。具备日志自动分卷压缩功能。

QMacros
QMacros 是一套跨平台的模板替换库，与原始的 StringReplace 函数相比，要替换的内容越多，性能优势越明显。具体相关内容可以查看 QMacros 专题 。

QAES
QAES 是一套跨平台的 Delphi 原生 AES 加密实现，封装的接口更加简单易用。更详细的信息，可以参考源码中相关注释说明。

QDigest
QDigest 是一套跨平台的 Delphi 原生 MD5 和 SHA 哈希摘要实现，同时也封装了简单易用的各种接口。更详细的信息，可以参考源码中相关注释说明。

QMemStatics
QMemStatics 是一套 Windows 下的内存分配分析工具，用于统计内存中不同尺寸的内存块分配情况，方便用户在设计服务程序时，合理的规划设计对象池的类型及大小。

QRBTree
QRBTree 包含了红黑树和哈希桶的 Delphi 实现，这两类数据结构的信息，请自行百度。本项目中许多单元如 QWorker 都引用了 QRBTree 单元。

QSimplePool
QSimple 提供一个简单的池实现，可以用于内存池和对象池。

QPlugin
QPlugin 是由群友 冰晰空气 为大家带来的一套开源插件框架，目前暂时只支持 Windows 平台，将来会融合 天地弦 的 MyBean 等开源框架思想和模式，形成跨平台的开源 Delphi 插件框架。

QDB
QDB 用于提供一套开源的跨平台数据库直接访问解决方式，计划支持 SQLite、 PostgreSQL、MSSQL、Oracle、MySQL 等常见数据库在跨平台直接访问。同时提供的 TQDataSet 会提供丰富复制、克隆、过滤、分组、导入、导出等接口的支持。目前该部分正在开发中，预计 2015 年初能够投入内测中。

QSocket
QSocket 用于提供一套开源的跨平台网络通讯支持框架，在 Windows 下将基于 IOCP ，在 Andriod 下将基于 EPoll，在OSX/iOS中将基于 KQueue 技术架构，同时会提供 P2P 和基于 UDP 的可靠网络传输实现（模拟TCP）。QSocket 还将提供和 QWorker 的信号相结合的网络信号解决方案，支持远程触发广播特定的信号。目前该框架在规划中，计划 2015 年投入开发和测试中。

QScript
QScript 用于提供一套高效的开源跨平台脚本解释引擎框架实现，目标是实现真编译环境50%以上的效率。但目前该框架同样只是在规划中，计划 2016 年投入研发。

【注】

本文档中说明的规划日期，只是一个大概的日期，作者受精力所限，不保证按时完成。一般来说，最终的功能要比规划的强大一些，但一切以最终实现为准，作者不承担您由于盲信作者而引起的任何责任。

最后，感谢大家对 QDAC 项目的关注，我们将努力带给大家最佳的 Delphi 数据访问组件，让大家在性能和稳定面前，不再难以取舍。
