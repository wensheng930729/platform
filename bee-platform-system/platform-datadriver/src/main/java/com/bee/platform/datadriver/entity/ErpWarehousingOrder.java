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
 * 成品入库主表
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-30
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
@TableName("erp_warehousing_order")
public class ErpWarehousingOrder extends Model<ErpWarehousingOrder> {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 成品入库编号
     */
    private String code;
    /**
     * 入库日期
     */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date warehousingTime;
    /**
     * 料批id
     */
    private Integer materialBatchId;
    /**
     * 料批名称
     */
    private String materialBatchName;

    /**
     * 炉号id
     */
    private Integer furnaceId;
    /**
     * 炉号
     */
    private String furnaceNumber;
    /**
     * 班次
     */
    private String classes;
    /**
     * 备注
     */
    private String remark;
    /**
     * 确认状态(0已保存，1已确认)
     */
    private Integer state;
    /**
     * 附件名
     */
    private String fileName;
    /**
     * 附件url
     */
    private String fileUrl;
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
    /**
     * 产成品名称
     */
    private String productName;
    /**
     * 产成品id
     */
    private Integer productId;

    /**
     * 仓库id
     */
    private Integer repositoryId;
    /**
     * 仓库名称
     */
    private String storeHouseName;
    /**
     * 单位
     */
    private String unit;
    /**
     * 入库数量
     */
    private BigDecimal amount;
    /**
     * 公司id
     */
    private Integer companyId;
    /**
     * 公司名称
     */
    private String companyName;

    /**
     * 创建时间
     */
    private Date createTime;

    /**
     * 化验单id
     */
    private Integer testReportId;
    /**
     * 化验单编号
     */
    private String testReportCode;




    @Override
    protected Serializable pkVal() {
        return this.id;
    }


}
