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
 * 企业关联用户审核日志表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
@TableName("enterprises_relation_user_check_log")
public class EnterprisesRelationUserCheckLog extends Model<EnterprisesRelationUserCheckLog> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 企业关联用户审核表id
     */
    private Integer enterpriseRelationUserCheckId;
    /**
     * 企业id
     */
    private Integer enterpriseId;
    /**
     * 企业名称
     */
    private String enterpriseName;
    /**
     * 操作人id
     */
    private Integer operateId;
    /**
     * 操作人名称
     */
    private String operateName;
    /**
     * 操作类型(0审核申请，1申请关联)
     */
    private Integer operateType;
    /**
     * 执行结果（0未通过，1已通过，2未审核）
     */
    private Integer operateResult;
    /**
     * 操作时间
     */
    private Date operateTime;
    /**
     * 拒绝原因
     */
    private String refuseReason;
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
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 修改人名称
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 数据状态(0无效，1有效)
     */
    private Integer status;



    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
