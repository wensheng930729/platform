package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName CommercialOpportunityQueryRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/24$ 10:26$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("商家信息列表查询信息")
public class CommercialOpportunityQueryRQ implements Serializable {
    private static final long serialVersionUID = -7429558818755367637L;
    
    /**
     * 客户类型（码表取值）
     */
    private String customerType;
    /**
     * 客户名称
     */
    private String customerName;
    /**
     * 销售员名称
     */
    private String saleUserName;
    /**
     * 当前阶段（码表取值）
     */
    private String phase;
}
