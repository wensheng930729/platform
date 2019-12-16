package com.bee.platform.datadriver.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.math.BigDecimal;
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
 * 料批主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_material_batch_order")
public class ErpMaterialBatchOrder extends Model<ErpMaterialBatchOrder> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 料批名称
     */
    private String materialBatchName;
    /**
     * 产成品id
     */
    private Integer productId;
    /**
     * 产成品名称
     */
    private String productName;
    /**
     * 产成品批次id
     */
    private Integer productBatchId;

    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;
    /**
     * 数量
     */
    private BigDecimal number;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建人企业id
     */
    private Integer creatorEnterpriseId;
    /**
     * 修改人id
     */
    private Integer modifierId;
    /**
     * 修改时间
     */
    private Date modifyTime;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    private Integer deleted;
    /**
     * 附件名称
     */
    private String fileName;
    /**
     * 附件url
     */
    private String fileUrl;
    /**
     * 状态(0失效，1生效)
     */
    private Integer state;
    /**
     * 综合入炉品位
     */
    private BigDecimal grade;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
