package com.bee.platform.user.entity;

import java.io.Serializable;

import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author xu.zheng123
 * @since 2019-04-29
 */
@Data
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("enterprises_apps_log")
public class EnterprisesAppsLog extends Model<EnterprisesAppsLog> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业产品申请主键
     */
    private Integer enterprisesAppsId;
    /**
     * 公司id
     */
    private Integer orgId;
    /**
     * 公司名称
     */
    private String orgName;
    /**
     * 产品id
     */
    private Integer appId;
    /**
     * 产品名称
     */
    private String appName;
    /**
     * 产品角色id
     */
    private Integer appRolesId;
    /**
     * 产品角色名称
     */
    private String appRolesName;
    /**
     * 操作类型（申请开通产品、审核申请）
     */
    private String operateType;
    /**
     * 审核状态( 未通过， 已通过,  审核中)
     */
    private String auditResult;
    /**
     * 拒绝原因
     */
    private String rejectReason;
    /**
     * 数据状态0-无效，1-有效
     */
    private Integer status;
    /**
     * 审核时间
     */
    private Date auditTime;
    /**
     * 审核人id
     */
    private Integer auditorId;
    /**
     * 审核人
     */
    private String auditor;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
}
