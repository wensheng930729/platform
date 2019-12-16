package com.bee.platform.costcontrol.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableField;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * <p>
 * 成本模拟基础配置
 * </p>
 *
 * @author liliang123
 * @since 2019-06-25
 */
@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_cost_simulation_cr")
public class ErpCostSimulationCr extends Model<ErpCostSimulationCr> {

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
     * 成本模拟名称
     */
    private String name;
    /**
     * 供应商id
     */
    private Integer supplierId;
    /**
     * 供应商
     */
    private String supplier;
    /**
     * 询盘人id
     */
    @TableField("inquirer_id")
    private Integer inquirerId;
    /**
     * 询盘人
     */
    @TableField("inquirer")
    private String inquirer;
    /**
     * 成本配置id
     */
    private Integer costAllocationId;
    /**
     * 成本配置(JSON)
     */
    private String costAllocation;
    /**
     * 原料
     */
    private String rawMaterial;
    /**
     * 交易港口
     */
    private String tradePort;
    /**
     * 现货单价
     */
    private BigDecimal spotUnitPrice;
    /**
     * 期货单价
     */
    private BigDecimal futuresUnitPrice;
    /**
     * 运费
     */
    private BigDecimal freight;
    /**
     * 港口服务费
     */
    private BigDecimal portServiceFee;
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
