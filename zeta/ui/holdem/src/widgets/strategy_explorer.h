#pragma once

#include "../solver/solution_store.h"
#include "../spot_document.h"
#include "../theme/theme.h"
#include "../viewmodels/strategy_view_model.h"

#include <QWidget>

#include <functional>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

class QComboBox;
class QGridLayout;
class QLabel;
class QPushButton;
class QTableWidget;
class QTreeWidget;
class QTreeWidgetItem;

namespace zeta::holdem::ui::widgets {

    /**
     * Root-artifact strategy explorer with matrix, filters, hand table, and inspector.
     */
    class strategy_explorer final : public QWidget {
    public:
        strategy_explorer(
            const spot& source,
            const solve_artifact& artifact,
            std::optional<solver::solution_store> solution,
            theme::density_metrics metrics,
            QWidget* parent = nullptr);

        strategy_explorer(
            const spot& source,
            const solve_artifact& artifact,
            theme::density_metrics metrics,
            QWidget* parent = nullptr);

        using locks_changed_callback = std::function<void(std::vector<cli::node_lock>)>;

        /** Enables lock editing; the callback receives the document's new lock list. */
        void set_locks_changed_callback(locks_changed_callback callback);

        /** The document's current locks, which may differ from those the artifact was solved with. */
        void set_current_locks(std::vector<cli::node_lock> locks);

        /** Lock path of the selected node, or nullopt when it is not a lockable decision. */
        [[nodiscard]] std::optional<std::vector<std::string>> active_lock_path() const;

        /** Adds or replaces a lock at the selected node (same hands) and reports the new list. */
        void apply_node_lock(cli::node_lock lock);

        /** Removes the locks written for exactly the selected node and reports the new list. */
        void remove_node_locks();

    private:
        void open_lock_dialog();
        void refresh_lock_controls();
        [[nodiscard]] QString node_lock_suffix(uint32_t graph_node_id) const;
        void create_layout();
        void refresh_filter();
        void refresh_node_context();
        void refresh_matrix();
        void refresh_hand_table();
        void refresh_detail(const QString& hand_class);
        void populate_tree_item(QTreeWidgetItem* item);
        [[nodiscard]] viewmodels::strategy_action_filter active_filter() const;
        [[nodiscard]] bool active_node_has_combo_strategy() const;

        viewmodels::strategy_view_model model_;
        solver::solution_store solution_;
        theme::density_metrics metrics_;
        QString selected_hand_class_;
        QString active_node_id_;
        QTreeWidget* node_tree_ = nullptr;
        QLabel* node_breadcrumb_ = nullptr;
        QLabel* node_state_ = nullptr;
        QTableWidget* node_action_table_ = nullptr;
        QComboBox* filter_selector_ = nullptr;
        QGridLayout* matrix_layout_ = nullptr;
        std::vector<QPushButton*> matrix_cells_;
        QTableWidget* hand_table_ = nullptr;
        QLabel* detail_title_ = nullptr;
        QTableWidget* detail_table_ = nullptr;

        std::vector<cli::node_lock> current_locks_;
        std::vector<cli::node_lock> solved_locks_;
        /** Artifact node id -> locks applied there in the solve. */
        std::unordered_map<uint32_t, std::vector<uint32_t>> solved_lock_indices_;
        /** Artifact node id -> (parent node id, lock path step into it). */
        std::unordered_map<uint32_t, std::pair<uint32_t, std::string>> lock_steps_;
        locks_changed_callback on_locks_changed_;
        QLabel* lock_status_ = nullptr;
        QPushButton* lock_button_ = nullptr;
        QPushButton* unlock_button_ = nullptr;
    };

}
