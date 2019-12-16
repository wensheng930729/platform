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
 * 工单信息表
 * </p>
 *
 * @author qhwang
 * @since 2019-04-25
 */
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@TableName("work_orders")
public class WorkOrders extends Model<WorkOrders> {

    private static final long serialVersionUID = -4909984486883507372L;

    /**
     * ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 工单编号
     */
    private String workOrderNumber;
    /**
     * 工单标题
     */
    private String workOrderTitle;
    /**
     * 优先级2重要1一般
     */
    private Integer priority;
    /**
     * 工单状态 1待平台处理、2平台处理中、3待用户确认、4已关闭
     */
    private Integer orderStatus;
    /**
     * 问题所属产品
     */
    private Integer belongApp;
    /**
     * 问题所属产品名称
     */
    private String belongAppName;
    /**
     * 提交人所属公司
     */
    private Integer belongCompany;
    /**
     * 提交人所属公司名称
     */
    private String belongCompanyName;
    /**
     * 提交人在所属公司角色名称
     */
    private String roleName;
    /**
     * 问题描述
     */
    private String problemDescription;
    /**
     * 联系手机号码
     */
    private String phone;
    /**
     * 附件
     */
    private String enclosure;
    /**
     * 提交时间
     */
    private Date submitTime;
    /**
     * 最近受理时间
     */
    private Date recentAcceptTime;
    /**
     * 受理人id
     */
    private Long acceptId;
    /**
     * 受理人
     */
    private String acceptor;
    /**
     * 数据状态0删除1正常
     */
    private Integer status;
    /**
     * 创建人id
     */
    private Long createId;
    /**
     * 创建人
     */
    private String creator;
    /**
     * 创建人头像
     */
    private String createHead;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改人id
     */
    private Long modifyId;
    /**
     * 修改人名称
     */
    private String modifier;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 其他信息
     */
    private String remark;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
