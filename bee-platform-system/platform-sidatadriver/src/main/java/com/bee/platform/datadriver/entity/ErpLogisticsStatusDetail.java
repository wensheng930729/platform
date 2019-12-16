package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

/**
 * <p>
 * 物流状态明细
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-07-25
 */
@Getter
@Setter
@ToString
@Accessors(chain=true)
@NoArgsConstructor
public class ErpLogisticsStatusDetail extends Model<ErpLogisticsStatusDetail> {

    private static final long serialVersionUID = 1L;

    /**
     * 物流明细id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品名称
     */
    private String productName;
    /**
     * 物流订单id
     */
    private Integer orderId;
    /**
     * 车牌号
     */
    private String plateNumber;
    /**
     * 合同重量
     */
    private BigDecimal contractWeight;
    
    /**
     * 载货重量
     */
    private BigDecimal loadingWeight;
    /**
     * 计量单位
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
     * 逻辑删除
     */
    private Integer deleted;
    
    /**
     * 物流订单状态：0是没有发货，1是在途中，2是已收货
     */
    private Integer status;
    
	/**
	 * "收货时间"
	 */
    private Date receivingTime;
    
    /**
     * ("批次id")
     */
    private Integer batchId;
    
    /**
     * ("批次名称")
     */
    private String batchName;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }
    
    

}
