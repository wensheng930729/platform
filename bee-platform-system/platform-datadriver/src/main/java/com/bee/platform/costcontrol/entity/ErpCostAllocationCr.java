package com.bee.platform.costcontrol.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableField;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * erp成本小工具-成本配置
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_cost_allocation_cr")
public class ErpCostAllocationCr extends Model<ErpCostAllocationCr> {

    private static final long serialVersionUID = 1L;

    /**
     * 主键
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 成本配置名称
     */
    private String name;
    /**
     * 铬回收率
     */
    private BigDecimal chromiumRecoveryRate;
    /**
     * 铁回收率
     */
    private BigDecimal ironRecoveryRate;
    /**
     * 硅含量
     */
    private BigDecimal siliconContent;
    /**
     * 碳含量
     */
    private BigDecimal carbonContent;
    /**
     * 运输损耗
     */
    @TableField("transport_Loss")
    private BigDecimal transportLoss;
    /**
     * 外币汇率
     */
    private BigDecimal foreignExchangeRate;
    /**
     * 炉电单价
     */
    private BigDecimal furnaceElectricityUnitPrice;
    /**
     * 炉电吨耗
     */
    private BigDecimal furnaceElectricityTonConsume;
    /**
     * 动力电单价
     */
    private BigDecimal electricityUnitPrice;
    /**
     * 动力电耗
     */
    private BigDecimal electricityConsume;
    /**
     * 固定碳
     */
    private BigDecimal fixedCarbon;
    /**
     * 利用率
     */
    private BigDecimal utilizationRate;
    /**
     * 焦炭单价
     */
    private BigDecimal cokeUnitPrice;
    /**
     * 焦炭比例
     */
    private BigDecimal cokeRatio;
    /**
     * 兰炭单价
     */
    private BigDecimal semiCokeUnitPrice;
    /**
     * 兰炭比例
     */
    private BigDecimal semiCokeRatio;
    /**
     * 电极糊单价
     */
    private BigDecimal electrodePasteUnitPrice;
    /**
     * 电极糊吨耗
     */
    private BigDecimal electrodePasteTonConsume;
    /**
     * 硅石单价
     */
    private BigDecimal silicaUnitPrice;
    /**
     * 硅石吨耗
     */
    private BigDecimal silicaTonConsume;
    /**
     * 制造费用
     */
    private BigDecimal manufacturingCost;
    /**
     * 辅料易耗
     */
    private BigDecimal consumableAccessorie;
    /**
     * 直接人工
     */
    private BigDecimal directLabor;
    /**
     * 劳务
     */
    private BigDecimal labourService;
    /**
     * 管理费用
     */
    private BigDecimal managementCost;
    /**
     * 销售费用
     */
    private BigDecimal saleCost;
    /**
     * 财务费用
     */
    private BigDecimal financeCost;
    /**
     * 状态：1启动 0禁用
     */
    private Integer status;
    /**
     * 表示逻辑删除，1-是删除，0-不删除
     */
    private Integer deleted;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建人姓名
     */
    private String creator;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 删除时间
     */
    private Date deletedTime;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
