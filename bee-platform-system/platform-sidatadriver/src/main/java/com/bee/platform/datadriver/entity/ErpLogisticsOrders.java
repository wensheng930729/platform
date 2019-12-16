package com.bee.platform.datadriver.entity;

import java.io.Serializable;
import java.math.BigDecimal;

import com.baomidou.mybatisplus.enums.IdType;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.activerecord.Model;

/**
 * <p>
 * 物流订单
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_logistics_orders")
public class ErpLogisticsOrders extends Model<ErpLogisticsOrders> {

    private static final long serialVersionUID = 1L;

    /**
     * 物流订单ID
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 物流订单号
     */
    private String orderNumber;
    /**
     * 公司id
     */
    private Integer companyId;
    
    /**
     * 公司名字
     */
    private String companyName;
    /**
     * 物流类型
     */
    private Integer type;
    private Integer sourceOrderId;
    /**
     * 订单来源（相当与销售采购订单）
     */
    private String sourceOrder;
    /**
     * 签订日期
     */
    private Date signingTime;
    private Integer carrierId;
    /**
     * 承运商名称
     */
    private String carrierName;
    /**
     * 起始地
     */
    private String origin;
    /**
     * 到达地
     */
    private String destination;
    /**
     * 备注
     */
    private String remarks;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 修改人id
     */
    private Integer updateUser;
    /**
     * 物流订单状态：0是没有发货，1是在途中，2是已收货
     */
    private Integer status;
    /**
     * 预计到达天数
     */
    private String estimatedArrivalTime;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 修改时间
     */
    private Date updateTime;
    
    /**
     * 发货时间
     */
    private Date deliveryTime;
    /**
     * 付款状态
     */
    private Integer payStatus;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
