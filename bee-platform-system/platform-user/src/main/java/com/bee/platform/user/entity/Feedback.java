package com.bee.platform.user.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 意见反馈
 * </p>
 *
 * @author liliang123
 * @since 2019-04-28
 */
@Setter
@Getter
@ToString
@Accessors(chain = true)
public class Feedback extends Model<Feedback> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 建议人
     */
    private String adviceUser;
    /**
     * 建议类型
     */
    private Integer adviceType;
    /**
     * 建议标题
     */
    private String adviceTitle;
    /**
     * 是否查看（0未查看 1查看）
     */
    private Integer isCheck;
    /**
     * 意见内容
     */
    private String content;
    /**
     * 附件
     */
    private String files;
    /**
     * 是否有效（0无效 1有效）
     */
    private Integer status;
    /**
     * 提交时间
     */
    private Date createTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
