package com.bee.platform.costcontrol.entity;

import java.io.Serializable;

import java.math.BigDecimal;
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

import java.io.Serializable;

/**
 * <p>
 * 料批模拟成分表
 * </p>
 *
 * @author xin.huang
 * @since 2019-06-25
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_cost_material_simulation_element")
public class ErpCostMaterialSimulationElement extends Model<ErpCostMaterialSimulationElement> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 料批模拟id
     */
    private Integer simulationId;
    /**
     * 原料名称
     */
    private String materialName;
    /**
     * Cr2O3
     */
    private BigDecimal chromiumTrioxide;
    /**
     * FeO
     */
    private BigDecimal ironOxide;
    /**
     * MgO
     */
    private BigDecimal magnesia;
    /**
     * Al2O3
     */
    private BigDecimal aluminumOxide;
    /**
     * 铬铁比
     */
    private BigDecimal chromiumIronRatio;
    /**
     * 镁铝比
     */
    private BigDecimal magnesiumAluminumRatio;
    /**
     * 现货到厂价
     */
    private BigDecimal price;
    /**
     * 料批配比
     */
    private BigDecimal materialRatio;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date updateTime;
    /**
     * 0未删除，1删除
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
