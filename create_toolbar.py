#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Add (or remove) the "我的工具" group in the AutoCAD for Mac toolbar.

AutoCAD for Mac keeps its user interface in plain XML files:

    <roaming>/CUI/Toolbar.cui     the window toolbar  (merged with Autodesk's default)
    <roaming>/CUI/MainMenu.cui    the menu bar        (merged with Autodesk's default)
    <roaming>/CUI/MenuGroup.cui   command definitions (merged with Autodesk's default)

Each toolbar button references an <ActionDef UID="..."> from MenuGroup.cui, whose
Command attribute holds the command macro, e.g. "^C^C_MJA".

IMPORTANT: AutoCAD rewrites Toolbar.cui / MainMenu.cui with its own state when it
quits, so those two files may only be written while AutoCAD is NOT running.

Usage:
    python3 create_toolbar.py "<roaming>/CUI"            # add
    python3 create_toolbar.py "<roaming>/CUI" --remove   # remove
"""
import glob
import os
import re
import shutil
import sys
import uuid

BLOCK_UID = "ID_MyTools"
GROUP_LABEL = "我的工具"

# UID, 名称, 宏, 显示的命令名, 图标, 说明
# 宏里用真正的 LISP 命令名（保证按钮一定能跑），CLICommand 才是
# 提示里显示、用户实际输入的那个短命令。
ACTIONS = [
    ("MJ_ID_AREA", "面积周长", "^C^C_MJA", "MJ", "RCDATA_16_AREA",
     "点取区域内部一点自动生成边界，输出面积、周长并画红色加粗轮廓"),
    ("MJ_ID_PLOT", "批量打印", "^C^C_MJPLOTQ", "DY", "RCDATA_16_PRINT",
     "批量打印：框选图框后直接按上次的设置出图"),
    ("MJ_ID_TILE", "瓷砖铺贴", "^C^C_MJTILE", "PT", "RCDATA_16_BHATCH",
      "瓷砖铺贴：点封闭空间 → 输砖长/宽/美缝 → 点起点，生成双线砖缝网格"),
    ("MJ_ID_UNINSTALL", "卸载插件", "^C^C_MJUNINSTALL", "MJUNINSTALL", "RCDATA_16_ERASE",
     "卸载 MJArea 插件，并还原被改过的设置文件"),
]


def uid():
    return str(uuid.uuid4()).upper()


def menu_group_xml():
    out = ['<?xml version="1.0"?>', '<Root SchemaVersion="1">',
           '    <Collection UUID="%s">' % uid()]
    for u, name, cmd, cli, img, help_text in ACTIONS:
        out.append(
            '        <ActionDef UID="%s" Name="%s" Command="%s" Type="Tools"'
            ' CLICommand="%s" Image="%s" Help="%s"></ActionDef>'
            % (u, name, cmd, cli, img, help_text))
    out.append('    </Collection>')
    out.append('</Root>')
    out.append('')
    return "\n".join(out)


def toolbar_block():
    """toolbar shows the drawing tools only - uninstall lives in the menu,
    where it takes a deliberate click and cannot be hit by accident."""
    parts = ['<Collection UUID="%s" UID="%s" Label="%s">'
             % (uid(), BLOCK_UID, GROUP_LABEL)]
    for u, name, cmd, cli, img, help_text in ACTIONS:
        if name == "卸载插件":
            continue
        parts.append(
            '<Control UUID="%s" Type="CommandButton" Label="%s">'
            '<Action UUID="%s" ActionID="%s"></Action></Control>'
            % (uid(), name, uid(), u))
    parts.append('</Collection>')
    return "".join(parts)


def menu_xml(outer_uuid):
    out = ['<?xml version="1.0"?>', '<Root SchemaVersion="1">',
           '    <Collection UUID="%s">' % outer_uuid,
           '        <Collection Name="%s" UID="%s_MENU" UUID="%s">'
           % (GROUP_LABEL, BLOCK_UID, uid())]
    for u, name, cmd, cli, img, help_text in ACTIONS:
        if name == "卸载插件":
            out.append('            <Control Type="Separator" UUID="%s"/>' % uid())
        out.append('            <Control Name="%s" UUID="%s">' % (name, uid()))
        out.append('                <Action ActionID="%s" UUID="%s"/>' % (u, uid()))
        out.append('            </Control>')
    out.append('        </Collection>')
    out.append('    </Collection>')
    out.append('</Root>')
    out.append('')
    return "\n".join(out)


def system_menu_uuid():
    """outer Collection UUID of Autodesk's own menu bar file"""
    pats = ["/Applications/Autodesk/AutoCAD */AutoCAD *.app/Contents/Resources/PopMainMenu.cui",
            "/Applications/Autodesk/AutoCAD*/AutoCAD*.app/Contents/Resources/PopMainMenu.cui"]
    for pat in pats:
        for p in sorted(glob.glob(pat)):
            m = re.search(r'<Collection UUID="([^"]+)"', open(p, encoding="utf-8", errors="replace").read())
            if m:
                return m.group(1)
    return None


def strip_menu(text):
    return re.sub(r'<Collection [^>]*UID="%s_MENU"[^>]*>.*?</Collection>' % BLOCK_UID,
                  '', text, flags=re.S)


def strip_block(text):
    return re.sub(r'<Collection [^>]*UID="%s"[^>]*>.*?</Collection>' % BLOCK_UID,
                  '', text, flags=re.S)


def backup(path):
    bak = path + ".before-mjarea"
    if os.path.exists(path) and not os.path.exists(bak):
        shutil.copy2(path, bak)
        print("      备份：%s" % bak)


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        return 2
    cui = os.path.abspath(os.path.expanduser(sys.argv[1]))
    remove = "--remove" in sys.argv
    os.makedirs(cui, exist_ok=True)
    toolbar = os.path.join(cui, "Toolbar.cui")
    menu_group = os.path.join(cui, "MenuGroup.cui")

    # ---------------- toolbar ----------------
    if os.path.exists(toolbar):
        text = open(toolbar, encoding="utf-8").read()
        text = strip_block(text)
        if not remove:
            idx = text.rfind("</Collection>")
            if idx < 0:
                print("!! Toolbar.cui 结构异常，跳过工具栏（原文件未改动）")
                return 1
            text = text[:idx] + toolbar_block() + text[idx:]
        backup(toolbar)
        with open(toolbar, "w", encoding="utf-8") as f:
            f.write(text)
        print("      %s 工具栏：%s" % ("已移除" if remove else "已加入", toolbar))
    else:
        print("      没有找到 Toolbar.cui，跳过工具栏按钮")

    # ---------------- menu group (action definitions) ----------------
    if remove:
        if os.path.exists(menu_group):
            txt = open(menu_group, encoding="utf-8").read()
            if "MJ_ID_AREA" in txt:
                os.remove(menu_group)
                print("      已删除 %s" % menu_group)
            else:
                print("      MenuGroup.cui 不是本插件写的，保留原文件")
    else:
        backup(menu_group)
        with open(menu_group, "w", encoding="utf-8") as f:
            f.write(menu_group_xml())
        print("      已写入命令定义：%s" % menu_group)

    # ---------------- menu bar ----------------
    main_menu = os.path.join(cui, "MainMenu.cui")
    outer = system_menu_uuid()
    if remove:
        if os.path.exists(main_menu):
            txt = open(main_menu, encoding="utf-8").read()
            if BLOCK_UID + "_MENU" in txt:
                os.remove(main_menu)
                print("      已删除菜单：%s" % main_menu)
    elif outer:
        txt = ""
        if os.path.exists(main_menu):
            txt = strip_menu(open(main_menu, encoding="utf-8").read())
        if BLOCK_UID + "_MENU" not in txt:
            backup(main_menu)
            if not txt.strip():
                txt = menu_xml(outer)
            else:
                idx = txt.rfind("</Collection>")
                block = re.search(r'<Collection [^>]*UID="%s[^"]*".*?</Collection>' % BLOCK_UID,
                                  menu_xml(outer), re.S).group(0)
                txt = txt[:idx] + block + txt[idx:]
            with open(main_menu, "w", encoding="utf-8") as f:
                f.write(txt)
            print("      已加入菜单栏：%s" % main_menu)
    else:
        print("      没找到 AutoCAD 的菜单文件，跳过菜单栏")

    return 0


if __name__ == "__main__":
    sys.exit(main())
