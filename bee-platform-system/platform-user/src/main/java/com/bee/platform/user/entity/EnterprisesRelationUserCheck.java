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
 * 企业关联用户审核表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
@TableName("enterprises_relation_user_check")
public class EnterprisesRelationUserCheck extends Model<EnterprisesRelationUserCheck> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 企业名称
     */
    private String enterpriseName;
    /**
     * 用户id
     */
    private Integer userId;
    /**
     * 用户名
     */
    private String userName;
    /**
     * 手机号
     */
    private String phone;
    /**
     * 审核人id
     */
    private Integer checkId;
    /**
     * 审核人姓名
     */
    private String checkName;
    /**
     * 审核状态（0未通过 1已通过 2审核中）
     */
    private Integer checkStatus;
    /**
     * 申请理由
     */
    private String applyReason;
    /**
     * 拒绝原因
     */
    private String refusalReason;
    /**
     * 数据状态（0删除 1正常）
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 创建人名称
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新人id
     */
    private Integer modifyId;
    /**
     * 更新人
     */
    private String modifier;
    /**
     * 更新时间
     */
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
