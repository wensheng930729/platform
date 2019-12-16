package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import java.math.BigDecimal;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 辅材消耗表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_auxiliary_material_consumption")
public class ErpAuxiliaryMaterialConsumption extends Model<ErpAuxiliaryMaterialConsumption> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 辅材消耗时间
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date consumptionDate;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 炉号id
     */
    private Integer furnaceId;

    /**
     * 炉号
     */
    private String furnaceNumber;
    /**
     * 产量
     */
    private BigDecimal output;
    /**
     * 生产领料金额
     */
    private BigDecimal productionMoney;
    /**
     * 环保金额
     */
    private BigDecimal environmentalMoney;
    /**
     * 安全金额
     */
    private BigDecimal securityMoney;
    /**
     * 检修金额
     */
    private BigDecimal overhaulMoney;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建人企业id
     */
    private Integer creatorEnterpriseId;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;

    /**
     * 修改人id
     */
    private Integer modifierId;
    /**
     * 修改时间
     */
    private Date modifyTime;



    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
