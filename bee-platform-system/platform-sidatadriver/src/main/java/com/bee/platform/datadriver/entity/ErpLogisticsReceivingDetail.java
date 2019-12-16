package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

/**
 * <p>
 * 收货明细
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_logistics_receiving_detail")
public class ErpLogisticsReceivingDetail extends Model<ErpLogisticsReceivingDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * 收货明细id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 物流订单id
     */
    private Integer orderId;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 车牌号码
     */
    private String plateNumber;
    /**
     * 收货时间
     */
    private Date receivingTime;
    /**
     * 合同重量
     */
    private BigDecimal contractWeight;
    /**
     * 单位
     */
    private String unit;
    /**
     * 毛重
     */
    private BigDecimal roughWeight;
    /**
     * 皮重
     */
    private BigDecimal tare;
    /**
     * 净重量
     */
    private BigDecimal suttle;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 创建人id
     */
    private Integer createUser;
    /**
     * 修改人id
     */
    private Integer updateUser;
    /**
     * 逻辑删除标识
     */
    private Integer deleted;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
