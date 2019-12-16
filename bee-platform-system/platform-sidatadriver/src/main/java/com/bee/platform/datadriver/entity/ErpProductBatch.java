package com.bee.platform.datadriver.entity;

import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 产品批次分类项
 * </p>
 *
 * @author liliang123
 * @since 2019-07-12
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class ErpProductBatch extends Model<ErpProductBatch> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Integer id;
    /**
     * 产品id
     */
    private Integer productId;
    /**
     * 产品批次分类项名称
     */
    private String batchName;
    /**
     * 创建人id
     */
    private Integer creatorId;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 状态:1-启用,0-禁用
     */
    private Integer status;
    /**
     * 是否删除 0未删除 1删除
     */
    private Integer deleted;

    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
