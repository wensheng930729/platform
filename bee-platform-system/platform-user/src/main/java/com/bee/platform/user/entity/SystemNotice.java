package com.bee.platform.user.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @notes: 后台管理系统通知
 * @Author: junyang.li
 * @Date: 10:16 2019/5/6
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_system_notice")
public class SystemNotice extends Model<SystemNotice> {

    private static final long serialVersionUID = 1L;

    /**
     * 通知id
     */
    @TableId(value = "notice_id", type = IdType.AUTO)
    private Long noticeId;
    /**
     * 通知人id
     */
    private Integer notifierId;
    /**
     * 通知标题
     */
    private String noticeTitle;
    /**
     * 通知内容
     */
    private String noticeContent;
    /**
     * 是否阅读，0未读，1已读
     */
    private Integer read;
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
        return this.noticeId;
    }

}
