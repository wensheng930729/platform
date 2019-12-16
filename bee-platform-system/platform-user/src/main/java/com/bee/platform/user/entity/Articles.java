package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("articles")
public class Articles extends Model<Articles> {

    private static final long serialVersionUID = 1L;


    /**
     *  文章id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer orgId;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 文章标题
     */
    private String title;
    /**
     * 文章内容
     */
    private String content;
    /**
     * 点击量
     */
    private Integer hits;
    /**
     * 文章发布日期
     */
    private Date createAt;
    /**
     * 文章修改日期
     */
    private Date updateAt;
    private Integer type;
    private String depName;
    /**
     * 附件名称
     */
    private String attachmentName;
    /**
     * 附件URL
     */
    private String attachmentUrl;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

    @Override
    public String toString() {
        return "Articles{" +
        ", id=" + id +
        ", orgId=" + orgId +
        ", userId=" + userId +
        ", title=" + title +
        ", content=" + content +
        ", hits=" + hits +
        ", createAt=" + createAt +
        ", updateAt=" + updateAt +
        ", type=" + type +
        ", depName=" + depName +
        ", attachmentName=" + attachmentName +
        ", attachmentUrl=" + attachmentUrl +
        "}";
    }
}
