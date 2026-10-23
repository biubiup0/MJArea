#!/bin/bash
# ============================================================
#  MJArea 面积/周长标注插件  —— 卸载脚本 (AutoCAD for Mac)
# ============================================================
set -u

ADDINS="$HOME/Library/Application Support/Autodesk/ApplicationAddins"
DEST="$ADDINS/MJArea.bundle"
HERE="$(cd "$(dirname "$0")" && pwd)"

echo "===================================================="
echo "   MJArea 面积/周长标注插件   卸载"
echo "===================================================="
echo

if pgrep -x "AutoCAD" >/dev/null 2>&1; then
  echo "提示：AutoCAD 正在运行，卸载后请完全退出并重新启动 AutoCAD。"
  echo
fi

# ---------- 1. 插件包 ----------
if [ -d "$DEST" ]; then
  rm -rf "$DEST"
  echo "[1/4] 已删除插件包：$DEST"
else
  echo "[1/4] 插件包不存在，跳过。"
fi

# ---------- 2. Support 目录中的副本 ----------
SUPPORT=""
for d in "$HOME/Library/Application Support/Autodesk/"AutoCAD*/R*/roaming/@*@/Support ; do
  [ -d "$d" ] && SUPPORT="$d"
done

if [ -n "$SUPPORT" ]; then
  if [ -f "$SUPPORT/MJArea.lsp" ]; then
    rm -f "$SUPPORT/MJArea.lsp"
    echo "[2/4] 已删除：$SUPPORT/MJArea.lsp"
  else
    echo "[2/4] Support 目录中没有副本，跳过。"
  fi
  if [ -f "$SUPPORT/MJArea.ini" ]; then
    rm -f "$SUPPORT/MJArea.ini"
    echo "      已删除设置文件：$SUPPORT/MJArea.ini"
  fi

  # ---------- 3. 去掉各启动文件里的加载代码 ----------
  FOUND=0
  for LOADER in "$SUPPORT"/acad.lsp "$SUPPORT"/acad[0-9]*.lsp "$SUPPORT"/acaddoc.lsp ; do
    [ -f "$LOADER" ] || continue
    if grep -q "^;; >>> MJArea" "$LOADER" 2>/dev/null; then
      cp "$LOADER" "$LOADER.bak"
      awk '/^;; >>> MJArea/{skip=1} !skip{print} /^;; <<< MJArea/{skip=0}' \
          "$LOADER.bak" > "$LOADER"
      echo "      已清理：${LOADER}（备份 .bak）"
      # 安装时新建的加载文件，清完就只剩空白行了——直接删掉，别留在目录里
      if ! grep -q "[^[:space:]]" "$LOADER" 2>/dev/null; then
        rm -f "$LOADER"
        echo "        （该文件是安装时新建的，已删除）"
      fi
      FOUND=1
    fi
  done
  if [ "$FOUND" = "1" ]; then
    echo "[3/4] 已从启动文件里移除本插件的加载代码"
  else
    echo "[3/4] 启动文件里没有本插件的加载代码，跳过。"
  fi
  rm -f "$SUPPORT/MJArea-startup.log"
  rm -f "$ADDINS/卸载插件.command"

  # ---------- 3.5 去掉“我的工具”工具栏按钮 ----------
  CUI="$(dirname "$(dirname "$SUPPORT")")/CUI"
  if [ -d "$CUI" ]; then
    if command -v python3 >/dev/null 2>&1; then
      if [ -f "$HERE/create_toolbar.py" ]; then
        python3 "$HERE/create_toolbar.py" "$CUI" --remove
      else
        python3 - "$CUI" <<'PY'
import os, re, sys
cui = sys.argv[1]
tb = os.path.join(cui, "Toolbar.cui")
mg = os.path.join(cui, "MenuGroup.cui")
if os.path.exists(tb):
    t = open(tb, encoding="utf-8").read()
    t2 = re.sub(r'<Collection [^>]*UID="ID_MyTools"[^>]*>.*?</Collection>', '', t, flags=re.S)
    if t2 != t:
        open(tb, "w", encoding="utf-8").write(t2)
        print("      已移除工具栏按钮")
if os.path.exists(mg) and "MJ_ID_AREA" in open(mg, encoding="utf-8").read():
    os.remove(mg)
    print("      已删除 MenuGroup.cui")
PY
      fi
    else
      echo "      （没有 python3，请手动在 CUI 里删掉“我的工具”组）"
    fi
  fi

  # ---------- 4. 去掉写进 acadUser.pgp 的别名（MJ / 旧版 LI） ----------
  PGP="$SUPPORT/acadUser.pgp"
  if [ -f "$PGP" ] && grep -q -E "\*MJA|\*MJTSET|\*MJTILE" "$PGP" 2>/dev/null; then
    cp "$PGP" "$PGP.bak"
    grep -v "\*MJA" "$PGP" | grep -v "\*MJTSET" | grep -v "\*MJTILE" | grep -v "MJArea" > "$PGP.tmp"
    mv "$PGP.tmp" "$PGP"
    echo "[4/4] 已从 acadUser.pgp 中移除 MJ / PT / LI 别名（备份：acadUser.pgp.bak）"
  else
    echo "[4/4] acadUser.pgp 中没有本插件的别名，跳过。"
  fi
else
  echo "[2/4] 没有找到 Support 目录，跳过。"
  echo "[3/4] 跳过。"
  echo "[4/4] 跳过。"
fi

echo
echo "卸载完成。重新启动 AutoCAD 后 MJ / MJSET 等命令将不再存在，"
echo "LI 一直是原来的 LIST 命令，不受影响。"
echo "（图纸里已经生成的标注文字和红线属于图纸内容，不会被删除；"
echo "  如需清理，可在 AutoCAD 里用 MJCLR 命令，或按图层删除。）"
echo
read -r -p "按回车键关闭…" _ || true
