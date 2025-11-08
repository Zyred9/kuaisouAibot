package com.search.robots.beans.view;


import com.search.robots.database.enums.Dialogue;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 *
 *
 * @author zyred
 * @since 2025/11/3 21:53
 */
@Setter
@Getter
@Accessors(chain = true)
public class DialogueCtx {

    private Dialogue dialogue;
    private Long businessId;

    public DialogueCtx (Dialogue dialogue) {
        this.dialogue = dialogue;
    }

    public DialogueCtx (Dialogue dialogue, Long businessId) {
        this.dialogue = dialogue;
        this.businessId = businessId;
    }
}
