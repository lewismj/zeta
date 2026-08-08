#include "theme/theme_styles.h"

#include "theme/theme_registry.h"

namespace zeta::holdem::ui::theme {

    namespace {

        [[nodiscard]] QString q(const std::string& value)
        {
            return QString::fromStdString(value);
        }

    }

    QString style_sheet(const registered_theme& theme, const density_mode density)
    {
        const auto& t = theme.tokens;
        const auto metrics = metrics_for_density(density);
        return QStringLiteral(R"(
            QMainWindow, QWidget#documentRoot, QTabWidget::pane {
                background: %1;
                color: %7;
            }
            QMenuBar, QMenu, QToolBar#commandBar, QStatusBar {
                background: %3;
                color: %7;
                border-color: %5;
            }
            QMenuBar::item:selected, QMenu::item:selected, QToolButton:hover {
                background: %16;
            }
            QToolBar#commandBar {
                spacing: %22px;
                border-bottom: 1px solid %5;
            }
            QToolButton, QPushButton {
                background: %12;
                color: %1;
                border: 1px solid %12;
                border-radius: 4px;
                padding: %23px %24px;
                font-weight: 600;
            }
            QToolButton:hover, QPushButton:hover {
                background: %13;
                border-color: %13;
            }
            QToolButton:disabled, QPushButton:disabled {
                background: %3;
                color: %9;
                border-color: %5;
            }
            QTabBar::tab {
                background: %3;
                color: %9;
                padding: %23px %24px;
                border-right: 1px solid %5;
                border-top: 2px solid transparent;
            }
            QTabBar::tab:selected {
                background: %1;
                color: %7;
                border-top-color: %10;
            }
            QSplitter::handle {
                background: %5;
            }
            QFrame#solverPanel, QFrame#positionCard, QFrame#activePositionCard, QFrame#tableStatePanel, QWidget#documentRail, QWidget#inspectorPanel {
                background: %2;
                border: 1px solid %5;
                border-radius: 4px;
            }
            QWidget#documentRail {
                border-left: 0;
                border-top: 0;
                border-bottom: 0;
                border-radius: 0;
            }
            QLabel#panelTitle, QLabel#positionName, QLabel#railTitle, QLabel#spotSummaryHeader {
                color: %7;
                font-weight: 600;
            }
            QLabel#errorLabel, QLabel#rangeParseError {
                color: %27;
                font-weight: 600;
            }
            QLabel#mutedLabel, QLabel#actionText {
                color: %9;
            }
            QLabel#activeActionText {
                color: %21;
                font-weight: 600;
            }
            QLabel#evPositive {
                color: %15;
            }
            QLabel#evWarning {
                color: %20;
            }
            QListWidget#documentRailList {
                background: %3;
                color: %8;
                border: 1px solid %5;
                outline: 0;
            }
            QListWidget#documentRailList::item {
                padding: %23px %24px;
                border-bottom: 1px solid %5;
            }
            QListWidget#documentRailList::item:selected {
                background: %16;
                color: %7;
                border-left: 2px solid %10;
            }
            QPlainTextEdit, QTableWidget, QSpinBox, QDoubleSpinBox, QComboBox {
                background: %4;
                color: %7;
                border: 1px solid %5;
                selection-background-color: %16;
                selection-color: %7;
            }
            QPlainTextEdit#solveConsole {
                background: %3;
                color: %8;
                border-top: 1px solid %5;
            }
            QHeaderView::section {
                background: %3;
                color: %8;
                border: 0;
                border-bottom: 1px solid %5;
                padding: %23px;
            }
            QPushButton#rangeCellPrimary, QPushButton#rangeCellSelected {
                background: %18;
                color: %1;
                border: 1px solid %11;
                border-radius: 2px;
                padding: 3px;
                text-align: left top;
            }
            QPushButton#rangeCellSelected {
                background: %19;
                border-color: %21;
            }
            QPushButton#rangeCellMuted {
                background: %17;
                color: %9;
                border: 1px solid %5;
                border-radius: 2px;
                padding: 3px;
                text-align: left top;
            }
            QLabel#tableFelt {
                background: %3;
                border: 1px solid %6;
                border-radius: 70px;
                color: %7;
            }
            QLabel#seatCard, QLabel#heroSeatCard, QLabel#activeSeatCard, QLabel#activeHeroSeatCard {
                background: %3;
                color: %8;
                border: 1px solid %5;
                border-radius: 4px;
                padding: %23px;
            }
            QLabel#heroSeatCard {
                border-color: %11;
                color: %7;
            }
            QLabel#activeSeatCard, QLabel#activeHeroSeatCard {
                border-color: %21;
                color: %21;
                font-weight: 600;
            }
            QPushButton#callButton {
                background: %21;
                color: %1;
                border-color: %21;
                font-size: %25px;
                text-align: left;
                padding: %26px;
            }
            QPushButton#foldButton {
                background: %14;
                color: %1;
                border-color: %14;
                font-size: %25px;
                text-align: left;
                padding: %26px;
            }
        )")
            .arg(q(t.background_base))
            .arg(q(t.background_raised))
            .arg(q(t.background_sunken))
            .arg(q(t.background_input))
            .arg(q(t.border_subtle))
            .arg(q(t.border_strong))
            .arg(q(t.text_primary))
            .arg(q(t.text_secondary))
            .arg(q(t.text_muted))
            .arg(q(t.accent_primary))
            .arg(q(t.accent_secondary))
            .arg(q(t.action_primary))
            .arg(q(t.action_primary_hover))
            .arg(q(t.action_negative))
            .arg(q(t.ev_positive))
            .arg(q(t.selection))
            .arg(q(t.range_heat[0]))
            .arg(q(t.range_heat[2]))
            .arg(q(t.range_heat[3]))
            .arg(q(t.warning))
            .arg(q(t.success))
            .arg(metrics.toolbar_spacing)
            .arg(density == density_mode::compact ? 4 : 6)
            .arg(density == density_mode::compact ? 8 : 10)
            .arg(density == density_mode::compact ? 18 : 22)
            .arg(density == density_mode::compact ? 8 : 10)
            .arg(q(t.error));
    }

}
