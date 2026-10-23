#!/bin/bash
# ============================================================
#  MJArea 面积/周长标注插件  —— 安装脚本 (AutoCAD for Mac)
#  双击运行即可，不需要管理员密码。
# ============================================================
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
SRC="$HERE/MJArea.bundle"
ADDINS="$HOME/Library/Application Support/Autodesk/ApplicationAddins"
DEST="$ADDINS/MJArea.bundle"

echo "===================================================="
echo "   MJArea 面积/周长标注插件   安装 (AutoCAD for Mac)"
echo "===================================================="
echo

if [ ! -d "$SRC" ]; then
  echo "!! 找不到 MJArea.bundle，请把本脚本和 MJArea.bundle 放在同一文件夹里再运行。"
  echo
  read -r -p "按回车键关闭…" _ || true
  exit 1
fi

if pgrep -x "AutoCAD" >/dev/null 2>&1; then
  echo "提示：AutoCAD 正在运行。安装完成后请【完全退出并重新启动】AutoCAD。"
  echo
fi

# 找到 AutoCAD 的用户 Support 目录和版本号
SUPPORT=""
for d in "$HOME/Library/Application Support/Autodesk/"AutoCAD*/R*/roaming/@*@/Support ; do
  [ -d "$d" ] && SUPPORT="$d"
done
VER=""
if [ -n "$SUPPORT" ]; then
  VER=$(printf '%s' "$SUPPORT" | sed -n 's|.*/AutoCAD \([0-9][0-9]*\)/.*|\1|p')
fi

# ---------- 1/5 官方插件包 (.bundle) ----------
mkdir -p "$ADDINS"
if [ -d "$DEST" ]; then
  echo "[1/5] 发现旧版本，先删除：$DEST"
  rm -rf "$DEST"
fi
cp -R "$SRC" "$DEST"
echo "[1/5] 插件包已安装到："
echo "      $DEST"
echo

# ---------- 2/5 自动加载：三个启动文件都写一份 ----------
if [ -n "$SUPPORT" ]; then
  cp "$SRC/Contents/Resources/MJArea.lsp" "$SUPPORT/MJArea.lsp"
  LOG="$SUPPORT/MJArea-startup.log"
  for NAME in "acad.lsp" "acad${VER}.lsp" "acaddoc.lsp"; do
    [ -z "$NAME" ] && continue
    F="$SUPPORT/$NAME"
    if [ -f "$F" ]; then
      cp "$F" "$F.before-mjarea"
      # 去掉旧的 MJArea 块（两种历史标记都能匹配）
      awk '/^;; >>> MJArea/{skip=1} !skip{print} /^;; <<< MJArea/{skip=0}' \
          "$F.before-mjarea" > "$F"
    fi
    {
      echo ""
      echo ";; >>> MJArea loader >>>  (delete this whole block to disable)"
      echo "(setq mjarea:log (open \"$LOG\" \"a\"))"
      echo "(if mjarea:log (progn (write-line \"$NAME\" mjarea:log) (close mjarea:log)))"
      echo "(if (findfile \"MJArea.lsp\") (load \"MJArea.lsp\" nil))"
      echo ";; <<< MJArea loader <<<"
    } >> "$F"
    echo "      已写入自动加载：$F"
  done
  echo "[2/5] 程序副本：$SUPPORT/MJArea.lsp"
  echo "      执行日志：${LOG}（记录哪个启动文件生效）"
else
  echo "[2/5] 没有找到 AutoCAD 用户 Support 目录，跳过自动加载。"
fi
echo

# ---------- 3/5 快捷键 MJ + 让 LI 保持 LIST ----------
#  LI 恢复成 AutoCAD 自带的 LIST；插件用 MJ 作为快捷键（MJ -> MJA）。
PGP="$SUPPORT/acadUser.pgp"
if [ -n "$SUPPORT" ] && [ -f "$PGP" ]; then
  # 去掉早期版本留下的 LI 别名
  if grep -q "^\s*LI\s*,\s*\*MJA" "$PGP" 2>/dev/null; then
    cp "$PGP" "$PGP.before-mjarea"
    grep -v "^\s*LI\s*,\s*\*MJA" "$PGP" | grep -v "MJArea 面积/周长标注插件" > "$PGP.tmp"
    mv "$PGP.tmp" "$PGP"
    echo "[3/5] 已把 LI 还原给 LIST"
  fi
  # 加上 MJ 快捷键
  if grep -q "^\s*MJ\s*,\s*\*MJA" "$PGP" 2>/dev/null; then
    echo "[3/5] MJ 快捷键已经存在，跳过"
  elif grep -q "^\s*MJ\s*," "$PGP" 2>/dev/null; then
    echo "[3/5] 注意：acadUser.pgp 里 MJ 已经被别的命令占用，未覆盖（可直接输入 MJA）。"
  else
    cp "$PGP" "$PGP.mjarea.bak"
    {
      echo ""
      echo "; MJArea 面积周长标注插件：MJ = 面积/周长标注快捷键"
      echo "MJ,        *MJA"
    } >> "$PGP"
    echo "[3/5] 已添加 MJ 快捷键（指向插件命令 MJA）："
    echo "      $PGP"
    echo "      备份：$PGP.mjarea.bak"
    echo "      提示：别名在启动时读取，改完请重启 AutoCAD，或在 CAD 里执行 REINIT 选 PGP。"
  fi
  # 加上 PT 快捷键（瓷砖铺贴）
  if grep -q "^\s*PT\s*,\s*\*MJTSET" "$PGP" 2>/dev/null; then
    cp "$PGP" "$PGP.mjarea.bak"
    sed -i '' 's/^\([[:space:]]*PT[[:space:]]*,[[:space:]]*\)\*MJTSET/\1*MJTILE/' "$PGP"
    sed -i '' 's/; MJArea 瓷砖铺贴：PT = 瓷砖设置快捷键/; MJArea 瓷砖铺贴：PT = 瓷砖铺贴快捷键/' "$PGP"
    echo "[3/5] 已把旧的 PT -> MJTSET 改为 PT -> MJTILE（瓷砖铺贴）"
  elif grep -q "^\s*PT\s*,\s*\*MJTILE" "$PGP" 2>/dev/null; then
    echo "[3/5] PT 瓷砖铺贴快捷键已经存在，跳过"
  elif grep -q "^\s*PT\s*," "$PGP" 2>/dev/null; then
    echo "[3/5] 注意：acadUser.pgp 里 PT 已经被别的命令占用，未覆盖（瓷砖可直接输入 MJTILE）。"
  else
    cp "$PGP" "$PGP.mjarea.bak"
    {
      echo ""
      echo "; MJArea 瓷砖铺贴：PT = 瓷砖铺贴快捷键"
      echo "PT,        *MJTILE"
    } >> "$PGP"
    echo "[3/5] 已添加 PT 快捷键（瓷砖铺贴，指向 MJTILE）"
  fi
else
  echo "[3/5] 没有找到 acadUser.pgp，跳过快捷键（可直接输入 MJA）。"
fi
echo

# ---------- 4/5 设置：文字高度统一 100 ----------
if [ -n "$SUPPORT" ]; then
  INI="$SUPPORT/MJArea.ini"
  if [ -s "$INI" ]; then
    cp "$INI" "$INI.bak"
    if grep -q "^txtheight=" "$INI"; then
      sed -i '' 's/^txtheight=.*/txtheight=100.000000/' "$INI"
    else
      printf 'txtheight=100.000000\n' >> "$INI"
    fi
    echo "[4/5] 已把标注文字高度统一设为 100（原设置备份为 MJArea.ini.bak）"
  else
    cp "$HERE/MJArea.ini" "$INI"
    echo "[4/5] 已写入默认设置：文字高度 100、单位毫米、红线 0.70mm"
  fi
else
  echo "[4/5] 跳过设置文件。"
fi
echo

# ---------- 4.5/5 “我的工具” 工具栏 ----------
CUI=""
if [ -n "$SUPPORT" ]; then
  CUI="$(dirname "$(dirname "$SUPPORT")")/CUI"
fi
if [ -n "$CUI" ] && [ -d "$CUI" ] && command -v python3 >/dev/null 2>&1; then
  if pgrep -x "AutoCAD" >/dev/null 2>&1; then
    echo "[4.5/5] ⚠️ AutoCAD 正在运行 —— 跳过工具栏/菜单写入。"
    echo "      AutoCAD 退出时会用自己的状态覆盖工具栏文件，"
    echo "      所以请【完全退出 AutoCAD】后，再运行一次本安装脚本，"
    echo "      然后再打开 AutoCAD，「我的工具」就会出现在工具栏和菜单栏里。"
    SKIP_CUI=1
  else
    SKIP_CUI=0
  fi
  if [ "${SKIP_CUI:-0}" = "0" ]; then
    echo "[4.5/5] 安装“我的工具”：工具栏 = 面积周长 / 批量打印 / 瓷砖铺贴，菜单栏再加“卸载插件”"
    python3 "$HERE/create_toolbar.py" "$CUI"
  fi
  cp "$HERE/卸载插件.command" "$ADDINS/卸载插件.command"
  chmod +x "$ADDINS/卸载插件.command"
  echo "      卸载程序已放到：$ADDINS/卸载插件.command"
elif [ -n "$CUI" ]; then
  echo "[4.5/5] 未找到 python3，跳过工具栏按钮（其他功能不受影响）"
else
  echo "[4.5/5] 跳过工具栏按钮。"
fi
echo

# ---------- 5/5 完成 ----------
echo "[5/5] 安装完成。"
echo
echo "接下来："
echo "  1) 完全退出 AutoCAD（Cmd+Q），再重新打开；"
echo "  2) 命令行输入  MJ   回车 → 框选房间/区域，即出面积周长 + 红色加粗轮廓；"
echo "  3) PT 瓷砖铺贴（点封闭空间 → 填砖长/宽/美缝 → 点起点即生成）；"
echo "     M / MJTSET 可修改已铺贴的图（改尺寸重画）或改默认参数；"
echo "  4) MJINFO 自检 / MJHELP 帮助 / MJSET 改设置 / MJCLR 清除标注。"
echo
echo "LI 仍然是原来的 LIST（列表查看）命令，不受影响。"
echo "卸载请双击“卸载插件.command”。"
echo
read -r -p "按回车键关闭…" _ || true
