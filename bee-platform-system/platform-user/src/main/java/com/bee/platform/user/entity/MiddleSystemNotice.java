package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 中台系统通知
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-09
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
@TableName("middle_system_notice")
public class MiddleSystemNotice extends Model<MiddleSystemNotice> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 通知人id
     */
    private Integer notifierId;
    /**
     * 通知标题
     */
    private String title;
    /**
     * 通知内容
     */
    private String content;
    /**
     * 是否阅读，0未读，1已读
     */
    private Integer isRead;
    /**
     * 是否有效，0无效，1有效
     */
    private Integer status;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date updateTime;



    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
