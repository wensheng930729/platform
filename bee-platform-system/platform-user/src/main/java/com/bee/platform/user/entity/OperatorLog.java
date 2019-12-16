package com.bee.platform.user.entity;

import java.io.Serializable;

import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @notes 操作日志实体
 * @Author junyang.li
 * @Date 10:17 2019/4/28
 **/
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("m_operator_log")
public class OperatorLog extends Model<OperatorLog> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 操作人id
     */
    private Integer operatorId;
    /**
     * 操作人角色
     */
    private Integer operatorRoleId;
    /**
     * 操作人角色名称
     */
    private String operatorRoleName;
    /**
     * 操作内容
     */
    private String operatorContent;
    /**
     * 操作时间
     */
    private Date operatorTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
